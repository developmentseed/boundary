use std::{cmp::Ordering, str::FromStr};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Parsing Error")]
    ParseError,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
/// Enum that holds a Type and enables how it should be treated for edge and infinity cases.
pub enum Boundary<T: PartialOrd> {
    EQ(T),
    LT(T),
    GT(T),
    Infinity,
    NegativeInfinity,
}

/// Ordering for Boundary
impl<T: PartialOrd> PartialOrd for Boundary<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Boundary::*;
        use Ordering::*;
        match (self, other) {
            (Infinity, Infinity) => Some(Equal),
            (Infinity, _) => Some(Greater),
            (NegativeInfinity, NegativeInfinity) => Some(Equal),
            (NegativeInfinity, _) => Some(Less),
            (_, Infinity) => Some(Less),
            (_, NegativeInfinity) => Some(Greater),
            (EQ(l) | GT(l), LT(r)) if l == r => Some(Greater),
            (EQ(l) | LT(l), GT(r)) if l == r => Some(Less),
            (LT(l), GT(r) | EQ(r)) if l == r => Some(Less),
            (GT(l), LT(r) | EQ(r)) if l == r => Some(Greater),
            (EQ(l) | GT(l) | LT(l), EQ(r) | GT(r) | LT(r)) => l.partial_cmp(r),
        }
    }
}

impl<T: Ord> Ord for Boundary<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        use Boundary::*;
        use Ordering::*;
        match (self, other) {
            (Infinity, Infinity) => Equal,
            (Infinity, _) => Greater,
            (NegativeInfinity, NegativeInfinity) => Equal,
            (NegativeInfinity, _) => Less,
            (_, Infinity) => Less,
            (_, NegativeInfinity) => Greater,
            (EQ(l) | GT(l), LT(r)) if l == r => Greater,
            (EQ(l) | LT(l), GT(r)) if l == r => Less,
            (LT(l), GT(r) | EQ(r)) if l == r => Less,
            (GT(l), LT(r) | EQ(r)) if l == r => Greater,
            (EQ(l) | GT(l) | LT(l), EQ(r) | GT(r) | LT(r)) => l.cmp(r),
        }
    }
}

impl<T> From<T> for Boundary<T>
where
    T: PartialOrd + ToString,
{
    fn from(v: T) -> Boundary<T> {
        let s = &v.to_string();
        if s == "inf" {
            Boundary::Infinity
        } else if s == "-inf" {
            Boundary::NegativeInfinity
        } else {
            Boundary::EQ(v)
        }
    }
}

impl<T> FromStr for Boundary<T>
where
    T: PartialOrd + FromStr,
{
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "inf" {
            Ok(Boundary::Infinity)
        } else if s == "-inf" {
            Ok(Boundary::NegativeInfinity)
        } else if let Some(stripped) = s.strip_prefix("<") {
            let val = stripped.parse::<T>().map_err(|_e| Error::ParseError)?;
            Ok(Boundary::LT(val))
        } else if let Some(stripped) = s.strip_prefix(">") {
            let val = stripped.parse::<T>().map_err(|_e| Error::ParseError)?;
            Ok(Boundary::GT(val))
        } else {
            let val = s.parse::<T>().map_err(|_e| Error::ParseError)?;
            Ok(Boundary::EQ(val))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_traits::Float;
    use rstest::*;

    #[rstest]
    fn test_float_casts() {
        let inf: f64 = Float::infinity();
        let ninf: f64 = Float::neg_infinity();
        let v = 1.234;
        let infb: Boundary<f64> = inf.into();
        let ninfb: Boundary<f64> = ninf.into();
        let vb: Boundary<f64> = v.into();
        assert_eq!(infb, Boundary::Infinity);
        assert_eq!(ninfb, Boundary::NegativeInfinity);
        assert_eq!(vb, Boundary::EQ(v));
    }

    #[rstest]
    #[case("0")]
    #[case("inf")]
    #[case("-inf")]
    #[case(">0")]
    #[case("<0")]
    fn test_str_to_float_casts(#[case] v: &str) {
        let b = v.parse::<Boundary<f64>>().unwrap();
        let m = match v {
            "0" => Boundary::EQ(0.0),
            "inf" => Boundary::Infinity,
            "-inf" => Boundary::NegativeInfinity,
            ">0" => Boundary::GT(0.0),
            "<0" => Boundary::LT(0.0),
            _ => unimplemented!(),
        };
        assert_eq!(b, m);
    }

    #[rstest]
    #[case("0")]
    #[case("inf")]
    #[case("-inf")]
    #[case(">0")]
    #[case("<0")]
    fn test_str_to_u32_casts(#[case] v: &str) {
        let b = v.parse::<Boundary<u32>>().unwrap();
        let m = match v {
            "0" => Boundary::EQ(0),
            "inf" => Boundary::Infinity,
            "-inf" => Boundary::NegativeInfinity,
            ">0" => Boundary::GT(0),
            "<0" => Boundary::LT(0),
            _ => unimplemented!(),
        };
        assert_eq!(b, m);
    }

    #[rstest]
    #[case("0")]
    #[case("inf")]
    #[case("-inf")]
    #[case(">0")]
    #[case("<0")]
    fn test_str_to_i32_casts(#[case] v: &str) {
        let b = v.parse::<Boundary<i32>>().unwrap();
        let m = match v {
            "0" => Boundary::EQ(0),
            "inf" => Boundary::Infinity,
            "-inf" => Boundary::NegativeInfinity,
            ">0" => Boundary::GT(0),
            "<0" => Boundary::LT(0),
            _ => unimplemented!(),
        };
        assert_eq!(b, m);
    }

    #[rstest]
    #[case("2020-01-01 00:00:00Z")]
    #[case("inf")]
    #[case("-inf")]
    #[case(">2020-01-01 00:00:00Z")]
    #[case("<2020-01-01 00:00:00Z")]
    fn test_str_to_timestamp_casts(#[case] v: &str) {
        use jiff::Timestamp;
        let b = v.parse::<Boundary<Timestamp>>().unwrap();
        let ts: Timestamp = "2020-01-01 00:00:00Z".parse().unwrap();
        let m = match v {
            "2020-01-01 00:00:00Z" => Boundary::EQ(ts),
            "inf" => Boundary::Infinity,
            "-inf" => Boundary::NegativeInfinity,
            ">2020-01-01 00:00:00Z" => Boundary::GT(ts),
            "<2020-01-01 00:00:00Z" => Boundary::LT(ts),
            _ => unimplemented!(),
        };
        assert_eq!(b, m);
    }

    #[rstest]
    #[case("0", "1.0")]
    #[case("0", "inf")]
    #[case("-inf", "inf")]
    #[case("-inf", "0")]
    #[case("<0", "0")]
    #[case("0", ">0")]
    #[case(">0", "inf")]
    #[case("<0", "inf")]
    #[case("-inf", "<0")]
    #[case("-inf", ">0")]
    fn test_float(#[case] l: Boundary<f64>, #[case] r: Boundary<f64>) {
        let l: Boundary<f64> = l;
        let r: Boundary<f64> = r;
        println!("l: {:?} r:{:?}", l, r);
        assert!(l < r);
        assert!(r > l);
    }
}
