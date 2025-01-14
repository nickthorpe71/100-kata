fn number(bus_stops:&[(i32,i32)]) -> i32 {
    bus_stops.iter().fold(0, |acc, x| acc + x.0 - x.1)
}

fn main() {
    let bus_stops = [(10,0), (3,5), (5,8)];
    println!("{}", number(&bus_stops));
}