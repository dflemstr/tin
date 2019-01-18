macro_rules! match_branches {
    ([$($branches:path,)*], $name:expr, ($($values:expr),*), |$param:ident| $body:expr, $default:expr) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$param| $body,
            (ref $param => $default,)
        )
    };
    ([$($branches:path,)*], $name:expr, ($($values:expr),*), |$($params:ident),*| $body:expr, $default:expr) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$($params),*| $body,
            (($(ref $params),*) => $default,)
        )
    };
    (inner: [], $name:expr, ($value:expr), |$($params:ident),*| $body:expr, ($($result:tt)*)) => {
        match $value {
            $($result)*
        }
    };
    (inner: [], $name:expr, ($($values:expr),*), |$($params:ident),*| $body:expr, ($($result:tt)*)) => {
        match ($($values,)*) {
            $($result)*
        }
    };
    (inner: [$branch:path, $($branches:path,)*], $name:expr, ($($values:expr),*), |$param:ident| $body:expr, ($($result:tt)*)) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$param| $body,
            ($branch(ref $param) => $body, $($result)*)
        )
    };
    (inner: [$branch:path, $($branches:path,)*], $name:expr, ($($values:expr),*), |$($params:ident),*| $body:expr, ($($result:tt)*)) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$($params),*| $body,
            (($($branch(ref $params)),*) => $body, $($result)*)
        )
    };
}

macro_rules! match_number {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| int: $body_int:expr, frac: $body_frac:expr) => {
        match_branches!(
            [
                value::Number::U8,
                value::Number::U16,
                value::Number::U32,
                value::Number::U64,
                value::Number::I8,
                value::Number::I16,
                value::Number::I32,
                value::Number::I64,
            ],
            $name,
            ($($value),*),
            |$($params),*| $body_int,
            match_branches!(
                [
                    value::Number::F32,
                    value::Number::F64,
                ],
                $name,
                ($($params),*),
                |$($params),*| $body_frac,
                Err(error::Error::RuntimeTypeConflict(format!("expected numbers of the same type but got {:?}", ($($params),*))))
            )
        )
    };
}

macro_rules! match_integral {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Number::U8,
                value::Number::U16,
                value::Number::U32,
                value::Number::U64,
                value::Number::I8,
                value::Number::I16,
                value::Number::I32,
                value::Number::I64,
            ],
            $name,
            ($($value),*),
            |$($params),*| $body,
            Err(error::Error::RuntimeTypeConflict(format!("expected integral numbers of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_fractional {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Number::F32,
                value::Number::F64,
            ],
            $name,
            ($($value),*),
            |$($params),*| $body,
            Err(error::Error::RuntimeTypeConflict(format!("expected fractional numbers of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_number_value {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| int: $body_int:expr, frac: $body_frac:expr) => {
        match_branches!(
            [
                value::Case::Number,
            ],
            $name,
            ($($value.case()),*),
            |$($params),*| match_number!(
                $name,
                ($($params),*),
                |$($params),*| int: $body_int, frac: $body_frac
            ),
            Err(error::Error::RuntimeTypeConflict(format!("expected number values of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_integral_value {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Case::Number,
            ],
            $name,
            ($($value.case()),*),
            |$($params),*| match_integral!(
                $name,
                ($($params),*),
                |$($params),*| $body
            ),
            Err(error::Error::RuntimeTypeConflict(format!("expected integral number values of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_fractional_value {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Case::Number,
            ],
            $name,
            ($($value.case()),*),
            |$($params),*| match_fractional!(
                $name,
                ($($params),*),
                |$($params),*| $body
            ),
            Err(error::Error::RuntimeTypeConflict(format!("expected fractional number values of the same type but got {:?}", ($($params),*))))
        )
    };
}
