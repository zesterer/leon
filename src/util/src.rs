use std::fmt;

#[derive(Copy, Clone, PartialEq)]
#[cfg(test)]
pub struct SrcLoc(pub usize);
#[derive(Copy, Clone, PartialEq)]
#[cfg(not(test))]
pub struct SrcLoc(usize);

impl SrcLoc {
    pub fn start() -> Self {
        Self(0)
    }

    pub fn min(self, other: Self) -> Self {
        Self(self.0.min(other.0))
    }

    pub fn max(self, other: Self) -> Self {
        Self(self.0.max(other.0))
    }

    pub fn in_context(&self, code: &str) -> (usize, usize) {
        let mut pos = self.0;
        for (i, line) in code.lines().enumerate() {
            if pos < line.len() + 1 {
                return (i, pos);
            }
            pos -= line.len() + 1;
        }
        (code.lines().count(), 0)
    }

    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }

    pub fn later_than(self, other: Self) -> bool {
        self.0 > other.0
    }
}

impl fmt::Debug for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<usize> for SrcLoc {
    fn from(pos: usize) -> Self {
        Self(pos)
    }
}

#[derive(Copy, Clone)]
#[cfg_attr(test, derive(PartialEq))]
pub enum SrcRegion {
    None,
    Range(SrcLoc, SrcLoc),
}

impl SrcRegion {
    pub fn none() -> Self {
        SrcRegion::None
    }

    pub fn single(loc: SrcLoc) -> Self {
        SrcRegion::Range(loc, loc.next())
    }

    pub fn range(from: SrcLoc, until: SrcLoc) -> SrcRegion {
        if from.0 < until.0 {
            SrcRegion::Range(from, until)
        } else {
            SrcRegion::None
        }
    }

    pub fn extend_to(self, limit: SrcLoc) -> Self {
        match self {
            SrcRegion::None => SrcRegion::None,
            SrcRegion::Range(from, until) => SrcRegion::Range(from, until.max(limit)),
        }
    }

    pub fn union(self, other: Self) -> Self {
        match (self, other) {
            (SrcRegion::None, _) => SrcRegion::None,
            (_, SrcRegion::None) => SrcRegion::None,
            (SrcRegion::Range(from_a, until_a), SrcRegion::Range(from_b, until_b)) =>
                SrcRegion::Range(from_a.min(from_b), until_a.max(until_b)),
        }
    }

    pub fn homogenize(self, other: Self) -> Self {
        match (self, other) {
            (SrcRegion::None, other) => other,
            (this, SrcRegion::None) => this,
            (this, _) => this,
        }
    }

    pub fn later_than(self, other: Self) -> bool {
        match (self, other) {
            (SrcRegion::Range(from_a, until_a), SrcRegion::Range(from_b, until_b)) =>
                until_a.later_than(until_b),
            _ => false,
        }
    }

    pub fn in_context(&self, code: &str) -> Option<((usize, usize), (usize, usize))> {
        match self {
            SrcRegion::Range(from, until) => Some((from.in_context(code), until.in_context(code))),
            SrcRegion::None => None,
        }
    }
}

impl fmt::Debug for SrcRegion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SrcRegion::None => write!(f, "<none>"),
            SrcRegion::Range(from, to) => write!(f, "{:?}:{:?}", from, to),
        }
    }
}

impl From<usize> for SrcRegion {
    fn from(pos: usize) -> Self {
        SrcRegion::Range(SrcLoc::from(pos), SrcLoc::from(pos + 1))
    }
}

impl From<(usize, usize)> for SrcRegion {
    fn from((from, to): (usize, usize)) -> Self {
        SrcRegion::Range(SrcLoc::from(from), SrcLoc::from(to))
    }
}
