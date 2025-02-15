package fominok.ideahelix

import com.intellij.openapi.project.Project
import com.intellij.openapi.wm.StatusBarWidget
import com.intellij.openapi.wm.StatusBarWidgetFactory

class ModeWidget : StatusBarWidgetFactory {
    override fun getId(): String {
        return ModePanel.ID
    }

    override fun getDisplayName(): String {
        return "Ideahelix Mode Widget"
    }

    override fun isAvailable(project: Project): Boolean {
        return true
    }

    override fun createWidget(project: Project): StatusBarWidget {
        return ModePanel()
    }

    override fun isEnabledByDefault(): Boolean {
        return true
    }
}

class ModePanel : StatusBarWidget.TextPresentation, StatusBarWidget {

    private var text: String = "Loading..."

    fun setText(text: String) {
        this.text = text
    }
    override fun getAlignment(): Float {
        return 0.0f
    }

    override fun getText(): String {
        return this.text
    }

    override fun getTooltipText(): String {
        return "Ideahelix mode"
    }

    override fun ID(): String {
        return ID
    }

    override fun getPresentation(): StatusBarWidget.WidgetPresentation {
        return this
    }

    companion object {
        @JvmField
        val ID: String = "IdeahelixModeWidget"
    }


}