package zio.codec

import com.github.ghik.silencer.silent
import zio.Chunk
import zio.internal.Stack

trait BitCodecModule extends CodecModule {
  type Input = Boolean

  val uint16: Codec[Int] = consume.repN(16).map(Equiv.Bits.UInt16)

  @silent
  private def compileCodec[A](codec: Codec[A]): Array[CodecVM] = ???

  @silent
  private def interpret(input: Chunk[Input], codec: Array[CodecVM]): Either[DecodeError, Any] = {
    import CodecVM._

    val stack: Stack[AnyRef] = Stack()

    var i: Int          = 0
    var inputIndex: Int = 0
    var r0: AnyRef      = null.asInstanceOf[AnyRef]

    while (i < codec.length) {
      val instr: CodecVM = codec(i)
      instr match {
        case Push(value) =>
          stack.push(value)

        case Read(min, max) =>
//          if (inputIndex + min < input.length

        case CheckSet(s) =>
          stack.push(s.contains(stack.pop()).asInstanceOf[AnyRef])

        case JumpEq(ifEqual, otherwise) =>
          if (stack.pop().eq(stack.pop())) i = ifEqual else i = otherwise

        case Construct1(f) =>
          stack.push(f(stack.pop()))

        case Construct2(f) =>
          stack.push(f(stack.pop(), stack.pop()))

        case Fail(err) =>
          return Left(DecodeError(err, i))

        case StoreRegister0 =>
          r0 = stack.pop()
      }
    }

    stack.pop() match {
      case null => Left(DecodeError("bug in our implementation, please report to us", 0))
      case v    => Right(v)
    }
  }
}
