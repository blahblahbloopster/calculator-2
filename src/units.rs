use std::ops::Add;

type Num = f64;

#[derive(Debug)]
pub enum Measure {
    Time, Length, Mass, Current, Temperature, Quantity, LuminousIntensity
}

#[derive(Debug)]
pub enum Unit {
    Base(String, Measure, Num),
    Derived { top: Vec<Unit>, bottom: Vec<Unit>, scale: Num }
}

pub struct UValue {
    value: Num,
    unit: Unit
}

//impl UValue {
//    fn simplify(self) -> Self {
//
//    }
//}

//impl Add for UValue {
//    type Output = UValue;
//
//    fn add(self, rhs: Self) -> Self::Output {
//        todo!()
//    }
//}
