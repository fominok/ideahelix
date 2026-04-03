package fominok.ideahelix

import com.intellij.openapi.actionSystem.ActionManager
import com.intellij.openapi.actionSystem.ActionPromoter
import com.intellij.openapi.actionSystem.ActionUpdateThread
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.actionSystem.AnActionWrapper
import com.intellij.openapi.actionSystem.CommonDataKeys
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.DumbAware
import java.awt.event.KeyEvent

class IdeaHelixShortcutAction : AnAction(), DumbAware {
    override fun getActionUpdateThread(): ActionUpdateThread = ActionUpdateThread.EDT

    override fun update(e: AnActionEvent) {
        val editor = e.getData(CommonDataKeys.EDITOR)
        val keyEvent = e.inputEvent as? KeyEvent
        e.presentation.isEnabled = editor != null && keyEvent != null && IdeaHelixNativeInput.shouldHandleShortcut(editor, keyEvent)
    }

    override fun actionPerformed(e: AnActionEvent) {
        val editor = e.getData(CommonDataKeys.EDITOR) ?: return
        val keyEvent = e.inputEvent as? KeyEvent ?: return
        val project = editor.project ?: return

        IdeaHelixClojure.pushEditorEvent(project, editor, keyEvent)
    }

    companion object {
        private const val ACTION_ID = "IdeaHelixShortcutAction"

        val instance: AnAction by lazy {
            AnActionWrapper(ActionManager.getInstance().getAction(ACTION_ID))
        }
    }
}

class IdeaHelixActionPromoter : ActionPromoter {
    override fun promote(actions: List<AnAction>, context: com.intellij.openapi.actionSystem.DataContext): List<AnAction>? {
        val shortcutAction = actions.firstOrNull { action ->
            action is AnActionWrapper && action.delegate is IdeaHelixShortcutAction
        } ?: return null

        return listOf(shortcutAction)
    }
}
