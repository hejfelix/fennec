package fennec

import java.util.UUID
import cats.Show

enum FormId:
  case Id(asUuid: UUID)
  case Name(asString: String)

object FormId:
  given Show[FormId] = _ match
    case Id(id)  => id.toString
    case Name(n) => n
end FormId
