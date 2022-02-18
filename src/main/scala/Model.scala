import indigo.shared.datatypes.Point
import indigo._

final case class Model(center: Point, dots: List[Dot]) {
  def addDot(dot: Dot): Model =
    this.copy(dots = dot :: dots)

  def update(timeDelta: Seconds): Model =
    this.copy(dots = dots.map(_.update(timeDelta)))
}
object Model {
  def initial(center: Point): Model = Model(center, Nil)
}

final case class Dot(orbitDistance: Int, angle: Radians) {
  def update(timeDelta: Seconds): Dot =
    this.copy(angle = angle + Radians.fromSeconds(timeDelta))
}