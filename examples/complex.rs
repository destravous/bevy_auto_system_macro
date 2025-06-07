use bevy::prelude::*;
use bevy_auto_system_macro::system;

pub fn main() {

}

// Test components
#[derive(Component)]
struct Health(u32);

#[derive(Component)]
struct Enemy;

system!(fn damage_enemies() {
    for mut enemy in #enemies.is<Enemy>.get_mut<Health>.get<Mana>[] {
        enemy.health.0 -= 5;
    }
});

fn damage_enemies_e(mut enemies_query: Query<(Entity, &mut Health), With<Enemy>>) {
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

#[derive(Component)]
struct Player;

#[derive(Component)]
struct Mana(u32);

system!(fn heal_players() {
    for #player in #players.is<Player>[] {
        if #player.get<Health>.0 < 50 {
            #player.get_mut<Health>.0 += 10;
            #player.get_mut<Mana>.0 -= 5;
        }
    }
});

fn heal_players_e(mut players_query: Query<(Entity, &mut Health, &mut Mana), With<Player>>) {
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

#[derive(Resource)]
struct TargetEntity(Entity);

system!(fn damage_target() {
    let target_id = #get_resource<TargetEntity>.0;
    if let Ok(mut target) = #enemies.get_mut<Health>[target_id] {
        target.health.0 -= 25;
    }

    #enemies[target_id].unwrap().health.0 -= 10;
});

fn damage_target_e(
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

#[derive(Component)]
struct Alive;
#[derive(Component)]
struct Stunned;
#[derive(Component)]
struct Position(u32);
#[derive(Component)]
struct Velocity(u32);
#[derive(Component)]
struct SpeedBoost {
    multiplier: u32
}

system!(fn process_units() {
    // only runs on units with changed velocity
    for #unit in #units.is<Alive>.is_not<Stunned>.changed<Velocity>[] {
        #unit.get_mut<Position>.0 += #unit.get_mut<Velocity>.0;

        if let Some(boost) = #unit.try_get<SpeedBoost> {
            #unit.get_mut<Velocity>.0 *= boost.multiplier;
        }
    }
});

fn process_units_e(
    mut units_query: Query<(Entity, &mut Position, &mut Velocity, Option<&SpeedBoost>), (With<Alive>, Without<Stunned>, Changed<Velocity>)>
){
    struct UnitsView<'a> {
        id: Entity,
        position: Mut<'a, Position>,
        velocity: Mut<'a, Velocity>,
        speed_boost: Option<&'a SpeedBoost>,
    }

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

#[derive(Component)]
struct SquadLeader;
#[derive(Component)]
struct HealPower(u32);

#[derive(Component)]
#[relationship_target(relationship = SquadMember)]
struct SquadMembers(Vec<Entity>);
#[derive(Component, Debug)]
#[relationship(relationship_target = SquadMembers)]
struct SquadMember(Entity);

#[derive(Component)]
struct Morale(u32);

system!(fn squad_heal() {
    for #leader in #leaders.is<SquadLeader>[] {
        let heal_amount = #leader.get<HealPower>.0;
        for #member in #leader.is<Morale>.via<SquadMembers>[] {
            #member.get_mut<Health>.0 += heal_amount;
            #member.get_mut<Morale>.0 += 5;
        }
    }
});

fn squad_heal_e(
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

#[derive(Resource)]
struct SpawnTimer(f32);

system!(fn spawn_enemies() {
    let time = #get_resource<Time>;
    let mesh_assets = #get_resource_mut<Assets<Mesh>>;
    let mut spawn_timer = #get_resource_mut<SpawnTimer>;

    spawn_timer.0 += time.delta_secs();

    if spawn_timer.0 > 1.0 {
        spawn_timer.0 = 0.0;

        #commands.spawn((
            Enemy,
            Health(100),
            Position(0),
        ));
    }
});

fn spawn_enemies_e(
    time: Res<Time>,
    mut spawn_timer: ResMut<SpawnTimer>,
    mut assets__mesh_: bevy::ecs::system::ResMut<Assets<Mesh>>,
    mut commands: Commands
) {
    let time = time;
    let mesh_assets = assets__mesh_;
    let mut spawn_timer = spawn_timer;

    spawn_timer.0 += time.delta_secs();

    if spawn_timer.0 > 1.0 {
        spawn_timer.0 = 0.0;

        commands.spawn((
            Enemy,
            Health(100),
            Position(0)
        ));
    }
}

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

fn check_player_e(players_query: Query<(Entity, &Health), With<Player>>) {
    let player_query = players_query;

    if player_query.is_empty() {
        println!("No players found!");
        return;
    }

    if let Ok((entity, health)) = player_query.single() {
        println!("Player {} has {} health", entity, health.0);
    }
}

#[derive(Component, PartialEq)]
enum State{
    Fleeing,
}

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
            #enemy.get_mut<Velocity>.0 *= 2;
        }
    }
});

fn complex_ai_e(mut enemies_query: Query<(Entity, &Health, &mut State, &mut Velocity), With<Enemy>>) {
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
            enemy.velocity.0 *= 2;
        }
    }
}

#[derive(Component)]
struct LowHealth;

system!(fn cleanup_dead() {
    for #unit in #units.is<Health>[] {
        if #unit.get<Health>.0 <= 0 {
            #unit.command().despawn();
        } else if #unit.get<Health>.0 < 20 {
            #unit.command().insert(LowHealth);
        }
    }
});

fn cleanup_dead_e(
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