use std::collections::HashSet;
use std::fmt;
use std::iter::Iterator;
use std::ops::{Index, IndexMut};

use chrono::prelude::*;
use itertools::Itertools;
use rayon::prelude::*;

use crate::error::Result;

const BLOCK: char = '#';
const EMPTY: char = '.';

type Point = (isize, isize);

#[derive(Clone, Debug)]
struct Map {
    map: Vec<Vec<char>>,
    x_bounds: isize,
    y_bounds: isize,
}

impl Map {
    fn new(map: Vec<Vec<char>>) -> Self {
        let x_bounds = map[0].len() as isize;
        let y_bounds = map.len() as isize;
        Self {
            map,
            x_bounds,
            y_bounds,
        }
    }

    fn contains(&self, point: Point) -> bool {
        let (x, y) = point;
        x >= 0 && x < self.x_bounds && y >= 0 && y < self.y_bounds
    }

    fn is_empty(&self, point: Point) -> bool {
        self[point] == EMPTY
    }

    fn get_all_points(&self) -> Vec<Point> {
        let mut points = Vec::new();

        for (y, row) in self.map.iter().enumerate() {
            for (x, _) in row.iter().enumerate() {
                points.push((y as isize, x as isize));
            }
        }

        points
    }
}

impl Index<Point> for Map {
    type Output = char;

    fn index(&self, index: Point) -> &Self::Output {
        let (x, y) = index;
        &self.map[y as usize][x as usize]
    }
}

impl IndexMut<Point> for Map {
    fn index_mut(&mut self, index: Point) -> &mut Self::Output {
        let (x, y) = index;
        &mut self.map[y as usize][x as usize]
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
struct Position {
    pos: Point,
    facing: usize,
}

impl Position {
    fn new(pos: Point) -> Self {
        Self { pos, facing: 0 }
    }

    fn turn(&mut self) {
        self.facing = (self.facing + 1) % 4;
    }

    fn move_forward(&mut self) {
        match self.facing {
            0 => self.pos.1 -= 1,
            1 => self.pos.0 += 1,
            2 => self.pos.1 += 1,
            3 => self.pos.0 -= 1,
            _ => unreachable!(),
        }
    }

    fn next_step(&self) -> Point {
        let (mut x, mut y) = self.pos;

        match self.facing {
            0 => y -= 1,
            1 => x += 1,
            2 => y += 1,
            3 => x -= 1,
            _ => unreachable!(),
        }

        (x, y)
    }

    fn blocked(&self, map: &Map) -> bool {
        let (x, y) = self.next_step();
        map.map
            .get(y as usize)
            .and_then(|row| row.get(x as usize))
            .map_or(false, |c| c == &BLOCK)
        // map[next_step] == BLOCK
    }

    fn walk_map(self, map: Map) -> MapWalker {
        MapWalker::new(self, map)
    }

    fn get_surrounding(&self) -> Vec<Point> {
        let (x, y) = self.pos;
        vec![(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let facing = match self.facing {
            0 => "n",
            1 => "e",
            2 => "s",
            3 => "w",
            _ => "?",
        };
        write!(f, "({}, {} - {})", self.pos.0, self.pos.1, facing)
    }
}

#[derive(Debug, Clone)]
struct MapWalker {
    cursor: Position,
    map: Map,
}

impl MapWalker {
    fn new(cursor: Position, map: Map) -> Self {
        Self { cursor, map }
    }
}

impl Iterator for MapWalker {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        // eprintln!("next {:?}", self.cursor);
        if !self.map.contains(self.cursor.pos) {
            // eprintln!("\toff map");
            return None;
        }

        if self.cursor.blocked(&self.map) {
            // eprintln!("\tblocked -- turning");
            self.cursor.turn();
        }

        self.cursor.move_forward();
        // eprintln!("\t-> {:?}", self.cursor);

        Some(self.cursor.clone())
    }
}

fn find_guard(map: &Map) -> Option<Point> {
    for (y, row) in map.map.iter().enumerate() {
        for (x, cell) in row.iter().enumerate() {
            if *cell == '^' || *cell == 'v' || *cell == '<' || *cell == '>' {
                return Some((x as isize, y as isize));
            }
        }
    }
    None
}

fn will_loop(to_block: &Point, map: &Map) -> bool {
    // log::debug!("will_loop: {:?}", to_block);

    let mut map = map.clone();
    let (x, y) = *to_block;
    map[*to_block] = BLOCK;

    let start = find_guard(&map).expect("the map data should contain a guard");
    let start = Position::new(start);
    // log::debug!("starting walk on {:?}", start);

    let mut seen = HashSet::new();
    let mut in_order = Vec::new();
    for point in start.walk_map(map) {
        in_order.push(point.clone());
        if seen.contains(&point) {
            let mut buffer = Vec::new();
            for pt in in_order {
                buffer.push(format!("{}", pt));
            }
            log::debug!("found loop {:?}: {}", to_block, buffer.join(", "));
            return true;
        }
        seen.insert(point);
    }

    false
}

pub fn day06<S: AsRef<str>>(input: S) -> Result<()> {
    let input = input.as_ref();
    let map = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let map = Map::new(map);
    let guard = Position::new(find_guard(&map).expect("guard not found"));

    let backbone = guard.walk_map(map.clone()).collect::<Vec<_>>();
    let uniq: HashSet<Position> = HashSet::from_iter(backbone.iter().cloned());
    let squares = backbone.iter().map(|p| p.pos).collect::<HashSet<_>>();
    println!("map bounds = {}, {}", map.x_bounds, map.y_bounds);
    println!("part 1: {}", squares.len());

    // let surrounding = uniq
    let surrounding = map.get_all_points();
    let surrounding = surrounding
        .iter()
        // .flat_map(|pos| pos.get_surrounding())
        .filter(|pt| map.contains(**pt))
        .filter(|pt| map.is_empty(**pt))
        .unique();

    let start_time = Utc::now();
    println!("starting: {}", start_time);
    let loop_count = surrounding
        // .par_iter()
        .filter(|pos| will_loop(pos, &map))
        .count();
    let end_time = Utc::now();
    println!("part 2: {}", loop_count);
    println!("elapsed: {}", end_time - start_time);

    Ok(())
}
