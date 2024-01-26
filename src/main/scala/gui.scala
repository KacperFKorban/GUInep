package guinep.internal

import scala.swing._
import scala.swing.event._
import java.awt.Color

def guinep(scripts: Map[String, Script]): Unit = {
  Swing.onEDT(guinepGo(scripts))
}

def guinepGo(scripts: Map[String, Script]): Unit = {
  // GUI Components
  val scriptList = new ListView(scripts.keys.toList)
  val mainPanel = new BoxPanel(Orientation.Vertical)
  val runButton = new Button("Run")
  val nameLabel = new Label("Choose a script")
  var selectedScript = ""

  def updateMainPanel(scriptName: String): Unit = {
    selectedScript = scriptName
    mainPanel.contents.clear()
    nameLabel.text = scriptName
    val inputPanel = new BorderPanel {
      layout(nameLabel) = BorderPanel.Position.North
      layout(new BoxPanel(Orientation.Vertical) {
        border = Swing.EmptyBorder(10, 10, 10, 10)
        scripts.get(scriptName).foreach { script =>
          script.inputs.foreach { _ =>
            contents += new TextField { columns = 1 }
            preferredSize = new Dimension(100, 1)
          }
        }
        contents += Swing.VStrut(10) // Add vertical space
        contents += runButton
      }) = BorderPanel.Position.Center
    }
    mainPanel.contents += inputPanel

    // Add a filler panel to fill the remaining space
    val filler = new BoxPanel(Orientation.Vertical) {
      background = Color.WHITE
      preferredSize = new Dimension(0, Int.MaxValue)
    }
    mainPanel.contents += filler


    mainPanel.revalidate()
    mainPanel.repaint()
  }

  val frame = new MainFrame {
    title = "Script Runner"
    contents = new BorderPanel {
      layout(new ScrollPane(scriptList)) = BorderPanel.Position.West
      layout(mainPanel) = BorderPanel.Position.Center
      minimumSize = new Dimension(400, 300)
    }

    // Handle script list selection
    scriptList.selection.reactions += {
      case ListSelectionChanged(_, _, _) =>
        updateMainPanel(scriptList.selection.items.headOption.getOrElse(""))
    }

    def getAllFieldInputs(panel: Panel): List[String] = {
      panel.contents.collect {
        case panel: Panel => getAllFieldInputs(panel)
        case textField: TextField => List(textField.text)
      }.toList.flatten
    }

    // Handle button click
    runButton.reactions += {
      case ButtonClicked(_) =>
        val inputs = getAllFieldInputs(mainPanel)
        val result = scripts.get(selectedScript).map(_.run(inputs)).getOrElse("Script not found")
        Dialog.showMessage(contents.head, result, "Result")
    }
  }

  frame.pack()
  frame.visible = true
}
