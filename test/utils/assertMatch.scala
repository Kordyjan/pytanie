package pytanie.test.utils

import munit.Assertions.assert

extension [T](actual: T)
  inline def assertMatch(inline pf: PartialFunction[T, Unit]) =
    assert(pf.isDefinedAt(actual), actual)
