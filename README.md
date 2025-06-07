[![Crates.io](https://img.shields.io/crates/v/bevy_auto_system_macro.svg)](https://crates.io/crates/bevy_auto_system_macro)
[![Documentation](https://docs.rs/bevy_auto_system_macro/badge.svg)](https://docs.rs/bevy_auto_system_macro)

Do you find updating your bevy system parameters tedious?
Do you wish you could just do `my_entity.get<Component>`?
Well, I've got the crate for you!

bevy_auto_system_macro is a crate that fully fills out system parameters *and* iteration loops, and then packages the variables in a nice IDE friendly ViewStruct.
So you can just write this:
```rust
system!(fn squad_heal() {
    for #leader in #leaders.is<SquadLeader>[] {
        let heal_amount = #leader.get<HealPower>.0;
        for #member in #leader.is<Morale>.via<SquadMembers>[] {
            #member.get_mut<Health>.0 += heal_amount * #get_resource<Time>.delta_secs_f64();
            #member.get_mut<Morale>.0 += 5;
        }
    }
});
```
And have it expand to:
```rust
fn squad_heal(leaders_query: Query<(Entity, &HealPower, &SquadMembers), (With<SquadLeader>, With<Morale>)>, mut member_query: Query<(Entity, &mut Health, &mut Morale)>, time: Res<Time>) {
    struct MemberView<'a> {
        id: Entity,
        health: Mut<'a, Health>,
        morale: Mut<'a, Morale>,
    }
    struct LeadersView<'a> {
        id: Entity,
        heal_power: &'a HealPower,
        squad_members: &'a SquadMembers,
    }
    for (entity, heal_power, squad_members) in leaders_query.iter() {
        let leader = LeadersView { id: entity, heal_power, squad_members };
        let heal_amount = leader.heal_power.0;
        for member_id in leader.squad_members.iter() {
            let Ok((entity, health, morale)) = member_query.get_mut(member_id) else { continue; };
            let mut member = MemberView { id: entity, health, morale };
            member.health.0 += heal_amount * time.delta_secs_f64();
            member.morale.0 += 5;
        }
    }
}
```


# Core Operations
**Entity Queries:**
- `#entities[]` - Iterate over all entities
- `#entities[id]` - Look up specific entity by ID
- `#entities` - Get query reference (no iteration)

**Component Access:**
- `.get<T>` - Immutable component access
- `.get_mut<T>` - Mutable component access
- `.try_get<T>` - Optional immutable access (returns Option)
- `.try_get_mut<T>` - Optional mutable access (returns Option)

**Query Filters:**
- `.is<T>` - Entity must have component T (With<T>)
- `.is_not<T>` - Entity must not have component T (Without<T>)
- `.changed<T>` - Component T was changed (Changed<T>)
- `.added<T>` - Component T was added (Added<T>)

**Resources:**
- `#get_resource<T>` - Access resource immutably
- `#get_resource_mut<T>` - Access resource mutably

**Commands:**
- `#commands.spawn(...)` - Spawn new entity
- `#entity.command().despawn()` - Despawn entity
- `#entity.command().insert(Component)` - Insert component

**Relationships:**
- `.via<RelationComponent>[]` - Iterate over related entities, currently only works in a for item in `#entity.via<RelationComponent>[]` kind of pattern.

# Examples

**Simple iteration over entities:**
```rust
system!(fn damage_enemies() {
    for enemy in #enemies.is<Enemy>.get_mut<Health>[] {
        enemy.health.0 -= 5;
    }
});
```

**Expands to:**
```rust
fn damage_enemies(mut enemies_query: Query<(Entity, &mut Health), With<Enemy>>) {
    struct EnemiesView<'a> {
        id: Entity,
        health: Mut<'a, Health>,
    }

    for mut enemy in enemies_query.iter_mut().map(|(entity, health)| {
        EnemiesView { id: entity, health }
    }) {
        enemy.health.0 -= 5;
    }
}
```

**You can use `#entity` (or whatever name you like) inside loops to extend the parent query:**
```rust
system!(fn heal_players() {
    for #player in #players.is<Player>[] {
        if #player.get<Health>.0 < 50 {
            #player.get_mut<Health>.0 += 10;
            #player.get_mut<Mana>.0 -= 5;
        }
    }
});
```

**Expands to:**
```rust
fn heal_players(mut players_query: Query<(Entity, &mut Health, &mut Mana), With<Player>>) {
    struct PlayersView<'a> {
        id: Entity,
        health: Mut<'a, Health>,
        mana: Mut<'a, Mana>,
    }

    for (entity, health, mana) in players_query.iter_mut() {
        let mut player = PlayersView { id: entity, health, mana };
        if player.health.0 < 50 {
            player.health.0 += 10;
            player.mana.0 -= 5;
        }
    }
}
```

**You can look up specific entities by ID:**
```rust
system!(fn damage_target() {
    let target_id = #get_resource<TargetEntity>.0;
    
    // Safe lookup - returns Option
    if let Ok(mut target) = #enemies.get_mut<Health>[target_id] {
        target.health.0 -= 25;
    }
    
    // Unsafe lookup - panics if not found
    #enemies[target_id].unwrap().health.0 -= 10;
});
```

**Expands to:**
```rust
fn damage_target(
    target_entity: Res<TargetEntity>,
    mut enemies_query: Query<(Entity, &mut Health)>
) {
    struct EnemiesView<'a> {
        id: Entity,
        health: Mut<'a, Health>,
    }

    let target_id = target_entity.0;

    if let Ok(mut target) = enemies_query.get_mut(target_id)
        .map(|(entity, health)| EnemiesView { id: entity, health }) {
        target.health.0 -= 25;
    }

    enemies_query.get_mut(target_id)
        .map(|(entity, health)| EnemiesView { id: entity, health })
        .unwrap().health.0 -= 10;
}
```

**Multiple filters and optional component access:**
```rust
system!(fn process_units() {
    // only runs on units with changed velocity
    for #unit in #units.is<Alive>.is_not<Stunned>.changed<Velocity>[] {
        #unit.get_mut<Position>.0 += #unit.get_mut<Velocity>.0;

        if let Some(boost) = #unit.try_get<SpeedBoost> {
            #unit.get_mut<Velocity>.0 *= boost.multiplier;
        }
    }
});
```

**Expands to:**
```rust
fn process_units(
    mut units_query: Query<(Entity, &mut Position, &mut Velocity, Option<&SpeedBoost>), (With<Alive>, Without<Stunned>, Changed<Velocity>)>
){
    struct UnitsView<'a> {
        id: Entity,
        position: Mut<'a, Position>,
        velocity: Mut<'a, Velocity>,
        speed_boost: Option<&'a SpeedBoost>,
    }

    // only runs on units with changed velocity
    for (entity, position, velocity, speed_boost) in units_query.iter_mut() {
        let mut unit = UnitsView {
            id: entity,
            position,
            velocity,
            speed_boost
        };

        unit.position.0 += unit.velocity.0;

        if let Some(boost) = unit.speed_boost {
            unit.velocity.0 *= boost.multiplier;
        }
    }
}
```

**Iterating over related entities:**
The `via` syntax is special; it
```rust
system!(fn squad_heal() {
    for #leader in #leaders.is<SquadLeader>[] {
        let heal_amount = #leader.get<HealPower>.0;
        
        for #member in #leader.via<SquadMembers>[] {
            #member.get_mut<Health>.0 += heal_amount;
            #member.get_mut<Morale>.0 += 5;
        }
    }
});
```

**Expands to:**
```rust
fn squad_heal(
    leaders_query: Query<(Entity, &HealPower, &SquadMembers), With<SquadLeader>>,
    mut members_query: Query<(Entity, &mut Health, &mut Morale)>
) {
    struct LeadersView<'a> {
        id: Entity,
        heal_power: &'a HealPower,
        squad_members: &'a SquadMembers,
    }

    struct MembersView<'a> {
        id: Entity,
        health: Mut<'a, Health>,
        morale: Mut<'a, Morale>,
    }

    for (entity, heal_power, squad_members) in leaders_query.iter() {
        let leader = LeadersView {
            id: entity,
            heal_power,
            squad_members
        };

        let heal_amount = leader.heal_power.0;

        for member_id in leader.squad_members.iter() {
            let Ok((entity, health, morale)) = members_query.get_mut(member_id) else {continue;};
            let mut member = MembersView { id: entity, health, morale };
            member.health.0 += heal_amount;
            member.morale.0 += 5;
        }
    }
}
```

## 6. Resources and Commands

**Using resources and spawning entities:**
```rust
system!(fn spawn_enemies() {
    let time = #get_resource<Time>;
    let mut spawn_timer = #get_resource_mut<SpawnTimer>;
    
    spawn_timer.0 += time.delta_secs();
    
    if spawn_timer.0 > 1.0 {
        spawn_timer.0 = 0.0;
        
        #commands.spawn((
            Enemy,
            Health(100),
            Position(Vec3::new(0.0, 0.0, 0.0)),
        ));
    }
});
```

**Expands to:**
```rust
fn spawn_enemies(
    time: Res<Time>,
    spawn_timer: ResMut<SpawnTimer>,
    mut commands: Commands
) {
    let time = time;
    let mut spawn_timer = spawn_timer;
    
    spawn_timer.0 += time.delta_secs();
    
    if spawn_timer.0 > 1.0 {
        spawn_timer.0 = 0.0;
        
        commands.spawn((
            Enemy,
            Health(100),
            Position(Vec3::new(0.0, 0.0, 0.0))
        ));
    }
}
```

## 7. Query References Without Iteration

**Using queries for single entity access or checking:**
```rust
system!(fn check_player() {
    // Just the query reference - useful for .single() or .is_empty()
    let player_query = #players.is<Player>.get<Health>;
    
    if player_query.is_empty() {
        println!("No players found!");
        return;
    }
    
    if let Ok((entity, health)) = player_query.single() {
        println!("Player {} has {} health", entity, health.0);
    }
});
```

**Expands to:**
```rust
fn check_player(players_query: Query<(Entity, &Health), With<Player>>) {
    let player_query = players_query;
    
    if player_query.is_empty() {
        println!("No players found!");
        return;
    }
    
    if let Ok((entity, health)) = player_query.single() {
        println!("Player {} has {} health", entity, health.0);
    }
}
```

## 8. Complex Query Merging

**Same entity accessed in different scopes:**
```rust
system!(fn complex_ai() {
    // First, check all enemies
    for #enemy in #enemies.is<Enemy>[] {
        if #enemy.get<Health>.0 < 20 {
            *#enemy.get_mut<State> = State::Fleeing;
        }
    }
    
    // Later, process fleeing enemies differently
    for #enemy in #enemies.is<Enemy>[] {
        if *#enemy.get<State> == State::Fleeing {
            #enemy.get_mut<Velocity>.0 *= 1.5;
        }
    }
});
```

**Expands to:**
```rust
fn complex_ai(mut enemies_query: Query<(Entity, &Health, &mut State, &mut Velocity), With<Enemy>>) {
    struct EnemiesView<'a> {
        id: Entity,
        health: &'a Health,
        state: Mut<'a, State>,
        velocity: Mut<'a, Velocity>,
    }

    // First loop
    for (entity, health, state, velocity) in enemies_query.iter_mut() {
        let mut enemy = EnemiesView {
            id: entity,
            health,
            state,
            velocity,
        };
        if enemy.health.0 < 20 {
            *enemy.state = State::Fleeing;
        }
    }

    // Second loop  
    for (entity, health, state, velocity) in enemies_query.iter_mut() {
        let mut enemy = EnemiesView {
            id: entity,
            health,
            state,
            velocity
        };
        if *enemy.state == State::Fleeing {
            enemy.velocity.0 *= 1.5;
        }
    }
}
```

## 9. Entity Commands

**Despawning entities and modifying components:**
```rust
system!(fn cleanup_dead() {
    for #unit in #units.is<Health>[] {
        if #unit.get<Health>.0 <= 0 {
            #unit.command().despawn();
        } else if #unit.get<Health>.0 < 20 {
            #unit.command().insert(LowHealth);
        }
    }
});
```

**Expands to:**
```rust
fn cleanup_dead(
    mut units_query: Query<(Entity, &Health)>,
    mut commands: Commands
) {
    struct UnitsView<'a> {
        id: Entity,
        health: &'a Health,
    }

    for (entity, health) in units_query.iter() {
        let unit = UnitsView { id: entity, health };

        if unit.health.0 <= 0 {
            commands.entity(unit.id).despawn();
        } else if unit.health.0 < 20 {
            commands.entity(unit.id).insert(LowHealth);
        }
    }
}
```

## License
This project is licensed under either the MIT License (see LICENSE-MIT) or the Apache License 2.0 (see LICENSE-APACHE), at your option.