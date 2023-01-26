package fennec

import java.nio.ByteBuffer

import scala.collection.mutable

package object app:

  extension (b: ByteBuffer)
    def size: Int = toList.size

    def toList: List[Byte] =
      val res = LazyList
        .unfold(())(_ =>
          if b.hasRemaining then Some((b.get, ()))
          else None,
        )
        .toList
      b.rewind()
      res
    end toList

    def toArray: Array[Byte] =
      val builder: mutable.ArrayBuilder[Byte] = Array.newBuilder[Byte]
      while b.hasRemaining do builder += b.get()
      b.rewind()
      builder.result()
  end extension
end app
