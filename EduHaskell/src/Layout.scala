package org.sufrin.util

/**
 * An algebra of boxes suitable for rendering
 * the text of an AST/TREE into nicely-nested
 * legible from.
 * 
 * Flow: 
 *       
 * 1. AST/TREE transformed to a Box -- a plan for eventually rendering it as a sequence of text lines
 *       
 * 2. theBox.render(width) renders theBox in the given width, breaking lines and nesting sub-boxes as needed.
 */

object Layout {

  implicit class LayoutText(val string: String) extends AnyVal {
    def t:    Layout.Box =  if (string.isEmpty) Box.Empty else Box.Text(string)
    def text: Layout.Box = if (string.isEmpty) Box.Empty else Box.Text(string)
  }

  implicit class LayoutBoxes(val boxes: Iterable[Box]) extends AnyVal {
    def sep(box: Box):   Box      = Box.sepBy(boxes, box)
    def punct(box: Box): Seq[Box] = Box.punctuate(boxes, box)
  }


  /** Presentation of the algebra */
  sealed trait Box {
    import Box._
    def +(that: Box):   Box = Cat(this, that)
    def <+>(that: Box): Box = this + space + that
    def nest(n: Int):   Box = Nest(n, this)
    def group:          Box = Group(this)
    def render(width: Int = 80): String = Renderer.render(width, this)
  }

  /**
   * Implementation of the operators.
   * NB: the toString methods
   */
  object Box {
    case object Empty extends Box
    final case class Text(s: String)                  extends Box
    final case class Line(alt: String, hard: Boolean) extends Box
    final case class Cat(left: Box, right: Box)       extends Box
    final case class Nest(indent: Int, box: Box)      extends Box
    final case class Group(box: Box)                  extends Box
    val empty: Box = Empty

    def text(s: String): Box = if (s.isEmpty) Empty else Text(s)

    val space: Box = Text(" ")

    /**
     * Breaking => NL; Flat => alt
     */
    def NL(alt: String = ""): Box = Line(alt, hard = false)

    /**
     *  Breaking => NL; Flat => " "
     */
    val softNL: Box = Line(" ", hard = false)

    /**
     * Breaks (even inside a group)
     */
    val hardNL: Box =  Line("", hard = true)

    def concat(boxes: Iterable[Box]):          Box = boxes.foldLeft(empty)(_ + _)

    def sepBy(boxes: Iterable[Box], sep: Box): Box = boxes.reduceOption(_ + sep + _).getOrElse(empty)

    def hbox(boxes: Iterable[Box]): Box = concat(boxes)

    def hsep(boxes: Iterable[Box]): Box = sepBy(boxes, space)

    def vbox(boxes: Iterable[Box]): Box = sepBy(boxes, hardNL)

    /**
     * Horizontal if it fits, vertical-ish if it does not.
     */
    def flow(boxes: Iterable[Box]): Box = sepBy(boxes, softNL).group

    def punctuate(boxes: Iterable[Box], punctuation: Box): Seq[Box] = {
      val xs = boxes.toSeq
      xs match {
        case Nil => Nil
        case _ =>
          xs.init.map(_ + punctuation) :+ xs.last
      }
    }

    def bracketed(open: String, body: Box, close: String): Box =
      text(open) + body + text(close)

    val softbreak: Box =
      Line("", hard = false)

    /**
     *  all vertical or all horizontal [ ... ]
     */
    def bracketed(bra: Box, items: Iterable[Box], ket: Box): Box = {
      val body  =  sepBy(items, text(",") + softNL)
      (bra + (softNL + body).nest(2) + softNL + ket).group
    }

    def bracketed(bra: Box, item: Box, ket: Box): Box = {
      (bra + (softNL + item + softNL).nest(2) + softNL + ket).group
    }

  }

  private object Renderer {
    import Box._

    private sealed trait Mode
    private case object  Flat   extends Mode
    private case object  Broken extends Mode

    private final case class Cmd(indent: Int, mode: Mode, box: Box)

    def render(width: Int, box: Box): String = {
      val out = new StringBuilder
      var col = 0
      var stack: List[Cmd] = List(Cmd(0, Broken, box))

      while (stack.nonEmpty) {
        val Cmd(indent, mode, current) = stack.head
        stack = stack.tail

        current match {
          case Empty =>
            ()

          case Text(s) =>
            out.append(s)
            col += s.length

          case Line(alt, hard) =>
            if (mode == Flat && !hard) {
              out.append(alt)
              col += alt.length
            } else {
              out.append('\n')
              out.append(" " * indent)
              col = indent
            }

          case Cat(left, right) =>
            stack = Cmd(indent, mode, left) :: Cmd(indent, mode, right) :: stack

          case Nest(extra, d) =>
            stack = Cmd(indent + extra, mode, d) :: stack

          case Group(d) =>
            val flatCmd = Cmd(indent, Flat, d)
            val chosen =
              if (fits(width - col, flatCmd :: stack)) Flat
              else Broken

            stack = Cmd(indent, chosen, d) :: stack
        }
      }

      out.toString
    }

    private def fits(remaining0: Int, cmds0: List[Cmd]): Boolean = {
      var remaining = remaining0
      var cmds = cmds0

      while (remaining >= 0 && cmds.nonEmpty) {
        val Cmd(indent, mode, box) = cmds.head
        cmds = cmds.tail

        box match {
          case Empty =>
            ()

          case Text(s) =>
            remaining -= s.length

          case Line(alt, hard) =>
            if (mode == Flat && !hard)
              remaining -= alt.length
            else
              return true

          case Cat(left, right) =>
            cmds = Cmd(indent, mode, left) :: Cmd(indent, mode, right) :: cmds

          case Nest(extra, d) =>
            cmds = Cmd(indent + extra, mode, d) :: cmds

          case Group(d) =>
            cmds = Cmd(indent, Flat, d) :: cmds
        }
      }

      remaining >= 0
    }
  }
}
