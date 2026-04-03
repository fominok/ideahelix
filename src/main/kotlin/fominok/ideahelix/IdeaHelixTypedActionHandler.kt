package fominok.ideahelix

import com.intellij.ide.IdeEventQueue
import com.intellij.openapi.actionSystem.DataContext
import com.intellij.openapi.editor.actionSystem.TypedActionHandler
import com.intellij.openapi.editor.impl.EditorImpl
import java.awt.event.KeyEvent

class IdeaHelixTypedActionHandler(
    private val originalHandler: TypedActionHandler,
) : TypedActionHandler {
    override fun execute(editor: com.intellij.openapi.editor.Editor, charTyped: Char, context: DataContext) {
        val project = editor.project
        val editorImpl = editor as? EditorImpl

        if (project == null || editorImpl == null || editorImpl.isOneLineMode) {
            originalHandler.execute(editor, charTyped, context)
            return
        }

        val modifiers = currentKeyEvent()?.modifiersEx ?: inferredModifiers(charTyped)
        val event = KeyEvent(
            editorImpl.contentComponent,
            KeyEvent.KEY_PRESSED,
            System.currentTimeMillis(),
            modifiers,
            KeyEvent.VK_UNDEFINED,
            charTyped,
        )

        val handled = IdeaHelixClojure.pushEditorEvent(project, editorImpl, event)
        if (!handled) {
            originalHandler.execute(editor, charTyped, context)
        }
    }

    private fun currentKeyEvent(): KeyEvent? {
        return IdeEventQueue.getInstance().trueCurrentEvent as? KeyEvent
    }

    private fun inferredModifiers(charTyped: Char): Int {
        return if (charTyped.isUpperCase() || charTyped in SHIFTED_SYMBOLS) {
            KeyEvent.SHIFT_DOWN_MASK
        } else {
            0
        }
    }

    companion object {
        private val SHIFTED_SYMBOLS = setOf(
            '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')',
            '_', '+', '{', '}', '|', ':', '"', '<', '>', '?',
        )
    }
}
