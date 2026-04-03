/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package fominok.ideahelix

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.editor.EditorFactory
import com.intellij.openapi.editor.event.EditorFactoryEvent
import com.intellij.openapi.editor.event.EditorFactoryListener
import com.intellij.openapi.editor.impl.EditorImpl
import com.intellij.openapi.editor.ex.EditorEventMulticasterEx
import com.intellij.openapi.editor.ex.FocusChangeListener
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.fileEditor.FileEditorManagerListener
import com.intellij.openapi.fileEditor.TextEditor
import com.intellij.openapi.project.Project
import com.intellij.openapi.startup.ProjectActivity
import com.intellij.openapi.vfs.VirtualFile

class Init : ProjectActivity {
    override suspend fun execute(project: Project) {
        val fileEditorManager = FileEditorManager.getInstance(project)
        val applicationManager = ApplicationManager.getApplication()

        IdeaHelixNativeInput.install()

        fileEditorManager.openFiles.forEach {
            applicationManager.invokeLater({
                val editor = (fileEditorManager.getEditors(it).firstOrNull() as? TextEditor)?.editor ?: return@invokeLater
                maybeRegisterNativeInput(project, editor)
                IdeaHelixClojure.focusEditor(project, editor)
            })
        }

        project.messageBus.connect().subscribe(FileEditorManagerListener.FILE_EDITOR_MANAGER, object: FileEditorManagerListener {
            override fun fileOpened(source: FileEditorManager, file: VirtualFile) {
                super.fileOpened(source, file)
                applicationManager.invokeLater({
                    val editor = source.getEditors(file).firstOrNull() as? TextEditor ?: return@invokeLater
                    maybeRegisterNativeInput(project, editor.editor)
                    IdeaHelixClojure.focusEditor(project, editor.editor)
                })
            }
        })

        val caster: EditorEventMulticasterEx = EditorFactory.getInstance().eventMulticaster as EditorEventMulticasterEx;
        caster.addFocusChangeListener(object : FocusChangeListener {
            override fun focusGained(editor: Editor) {
                super.focusGained(editor)
                applicationManager.invokeLater({
                    maybeRegisterNativeInput(project, editor)
                    IdeaHelixClojure.focusEditor(project, editor)
                })
            }
        }, project)

        EditorFactory.getInstance().addEditorFactoryListener(object : EditorFactoryListener {
            override fun editorReleased(event: EditorFactoryEvent) {
                val editor = event.editor
                val editorProject = editor.project ?: return
                if (editorProject != project) return

                IdeaHelixClojure.releaseEditor(project, editor)
            }
        }, project)
    }

    private fun maybeRegisterNativeInput(project: Project, editor: Editor) {
        (editor as? EditorImpl)?.let { IdeaHelixNativeInput.registerEditor(it, project) }
    }
}
