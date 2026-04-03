package fominok.ideahelix

import com.intellij.openapi.actionSystem.CustomShortcutSet
import com.intellij.openapi.actionSystem.KeyboardShortcut
import com.intellij.codeInsight.lookup.LookupManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.actionSystem.TypedAction
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import javax.swing.KeyStroke
import java.awt.event.KeyEvent

object IdeaHelixNativeInput {
    private val shortcutsRegisteredKey = Key.create<Boolean>("ideahelix.native.shortcuts.registered")

    private val shortcutKeyStrokes = buildList {
        addAll(
            listOf(
                KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_TAB, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_INSERT, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_UP, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_LEFT, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_RIGHT, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_HOME, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_END, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, 0),
                KeyStroke.getKeyStroke(KeyEvent.VK_SEMICOLON, KeyEvent.ALT_DOWN_MASK),
                KeyStroke.getKeyStroke(KeyEvent.VK_SEMICOLON, KeyEvent.ALT_DOWN_MASK or KeyEvent.SHIFT_DOWN_MASK),
                KeyStroke.getKeyStroke(KeyEvent.VK_TAB, KeyEvent.CTRL_DOWN_MASK),
            ),
        )
        addAll(modifiedLetterKeyStrokes(KeyEvent.CTRL_DOWN_MASK))
        addAll(modifiedLetterKeyStrokes(KeyEvent.ALT_DOWN_MASK))
    }.toTypedArray()

    private val registeredShortcutStrokes = shortcutKeyStrokes.toSet()

    fun install() {
        val typedAction = TypedAction.getInstance()
        if (typedAction.rawHandler !is IdeaHelixTypedActionHandler) {
            typedAction.setupRawHandler(IdeaHelixTypedActionHandler(typedAction.rawHandler))
        }
    }

    fun registerEditor(editor: EditorImpl, project: Project) {
        if (editor.isOneLineMode || editor.contentComponent.getClientProperty(shortcutsRegisteredKey) == true) {
            return
        }

        editor.contentComponent.putClientProperty(shortcutsRegisteredKey, true)
        val shortcuts = shortcutKeyStrokes.map { KeyboardShortcut(it, null) }.toTypedArray()
        IdeaHelixShortcutAction.instance.registerCustomShortcutSet(
            CustomShortcutSet(*shortcuts),
            editor.contentComponent,
            project,
        )
    }

    fun shouldHandleShortcut(editor: Editor, keyEvent: KeyEvent): Boolean {
        if (editor.isOneLineMode) return false

        val project = editor.project ?: return false
        val mode = IdeaHelixClojure.currentMode(project)
        val keyStroke = KeyStroke.getKeyStrokeForEvent(keyEvent)
        val lookupActive = LookupManager.getActiveLookup(editor) != null

        return when {
            keyEvent.keyCode == KeyEvent.VK_ESCAPE -> true
            mode == "insert" -> keyStroke in insertModeShortcutStrokes && lookupActive
            lookupActive -> false
            mode != "insert" -> keyStroke in modalShortcutStrokes
            else -> false
        }
    }

    private val insertModeShortcutStrokes = setOf(
        KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK),
        KeyStroke.getKeyStroke(KeyEvent.VK_P, KeyEvent.CTRL_DOWN_MASK),
    )

    private val modalShortcutStrokes = registeredShortcutStrokes - insertModeShortcutStrokes

    private fun modifiedLetterKeyStrokes(modifier: Int): List<KeyStroke> {
        return ('A'..'Z').map { char ->
            KeyStroke.getKeyStroke(char.code, modifier)
        }
    }
}
