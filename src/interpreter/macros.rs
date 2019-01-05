macro_rules! match_branches {
    ([$($branches:path,)*], $name:expr, ($($values:expr),*), |$param:ident| $body:expr, $default:expr) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$param| $body, $default,
            (ref $param => $default,)
        )
    };
    ([$($branches:path,)*], $name:expr, ($($values:expr),*), |$($params:ident),*| $body:expr, $default:expr) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$($params),*| $body, $default,
            (($(ref $params),*) => Err(Error::RuntimeTypeConflict(format!(
                concat!(
                    "operation ",
                    $name,
                    " not supported on values {:?}"
                ),
                ($($params),*)
            ))),)
        )
    };
    (inner: [], $name:expr, ($value:expr), |$($params:ident),*| $body:expr, $default:expr, ($($result:tt)*)) => {
        match $value {
            $($result)*
        }
    };
    (inner: [], $name:expr, ($($values:expr),*), |$($params:ident),*| $body:expr, $default:expr, ($($result:tt)*)) => {
        match ($($values,)*) {
            $($result)*
        }
    };
    (inner: [$branch:path, $($branches:path,)*], $name:expr, ($($values:expr),*), |$param:ident| $body:expr, $default:expr, ($($result:tt)*)) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$param| $body, $default,
            ($branch(ref $param) => $body, $($result)*)
        )
    };
    (inner: [$branch:path, $($branches:path,)*], $name:expr, ($($values:expr),*), |$($params:ident),*| $body:expr, $default:expr, ($($result:tt)*)) => {
        match_branches!(
            inner:
            [$($branches,)*],
            $name,
            ($($values),*),
            |$($params),*| $body, $default,
            (($($branch(ref $params)),*) => $body, $($result)*)
        )
    };
}

macro_rules! match_number {
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
                value::Number::F32,
                value::Number::F64,
            ],
            $name,
            ($($value),*),
            |$($params),*| $body,
            Err(Error::RuntimeTypeConflict(format!("expected numbers of the same type but got {:?}", ($($params),*))))
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
            Err(Error::RuntimeTypeConflict(format!("expected integral numbers of the same type but got {:?}", ($($params),*))))
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
            Err(Error::RuntimeTypeConflict(format!("expected fractional numbers of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_number_value {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Value::Number,
            ],
            $name,
            ($($value),*),
            |$($params),*| match_number!(
                $name,
                ($($params),*),
                |$($params),*| $body
            ),
            Err(Error::RuntimeTypeConflict(format!("expected number values of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_integral_value {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Value::Number,
            ],
            $name,
            ($($value),*),
            |$($params),*| match_integral!(
                $name,
                ($($params),*),
                |$($params),*| $body
            ),
            Err(Error::RuntimeTypeConflict(format!("expected integral number values of the same type but got {:?}", ($($params),*))))
        )
    };
}

macro_rules! match_fractional_value {
    ($name:expr, ($($value:expr),*), |$($params:ident),*| $body:expr) => {
        match_branches!(
            [
                value::Value::Number,
            ],
            $name,
            ($($value),*),
            |$($params),*| match_fractional!(
                $name,
                ($($params),*),
                |$($params),*| $body
            ),
            Err(Error::RuntimeTypeConflict(format!("expected fractional number values of the same type but got {:?}", ($($params),*))))
        )
    };
}
