import indigo._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object MyGameSandbox extends IndigoSandbox[Unit, Model] {

  val magnification = 2
  val config: GameConfig =
    GameConfig.default.withMagnification(magnification)

  val animations: Set[Animation] =
    Set()

  val assetName = AssetName("dots")

  val assets: Set[indigo.AssetType] = Set(
    AssetType.Image(assetName, AssetPath("assets/dots.png"))
  )
  val fonts: Set[FontInfo] =
    Set()

  val shaders: Set[Shader] =
    Set()

  def setup(assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(
      Model.initial(
        config.viewport.giveDimensions(magnification).center
      )
    )

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] = {
    case MouseEvent.Click(clickPoint) =>
      val adjustedPosition = clickPoint - model.center

      Outcome(
        model.addDot(
          Dot(
            Point.distanceBetween(model.center, clickPoint).toInt,
            Radians(
              Math.atan2(
                adjustedPosition.x.toDouble,
                adjustedPosition.y.toDouble
              )
            )
          )
        )
      )

    case FrameTick =>
      Outcome(model.update(context.delta))

    case _ =>
      Outcome(model)
  }
  def present(context: FrameContext[Unit], model: Model): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Graphic(Rectangle(0, 0, 32, 32), 1, Material.Bitmap(assetName)) ::
          drawDots(model.center, model.dots)
      )
    )

  def drawDots(
      center: Point,
      dots: List[Dot]
  ): List[Graphic[_]] =
    dots.map { dot =>
      val position = Point(
        x = (Math.sin(dot.angle.toDouble) * dot.orbitDistance + center.x).toInt,
        y = (Math.cos(dot.angle.toDouble) * dot.orbitDistance + center.y).toInt
      )

      Graphic(Rectangle(0, 0, 32, 32), 1, Material.Bitmap(assetName))
        .withCrop(Rectangle(16, 16, 16, 16))
        .withRef(8, 8)
        .moveTo(position)
    }
}
