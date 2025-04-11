import * as path from "path";
import * as util from "util";
import * as vscode from "vscode";
import {
    TextEditor,
    TextEditorEdit,
} from "vscode";

import {
    LanguageClient,
    LanguageClientOptions,
    MessageDirection,
    NotebookDocumentFilter,
    Position,
    ProtocolRequestType,
    RequestHandler,
    ServerOptions,
    TextDocumentFilter,
    TextDocumentIdentifier,
    TransportKind,
    uinteger,
} from "vscode-languageclient/node";

let client: LanguageClient;

type BlockAnnotationParams = {
    textDocument: TextDocumentIdentifier;
    method: string;
    block: uinteger;
};

type BlockAnnotation = {
    html: string;
    position: Position;
};

namespace BlockAnnotationRequest {
    export const method: "jackrabbit/blockAnnotation" = "jackrabbit/blockAnnotation";
    export const messageDirection: MessageDirection = MessageDirection.clientToServer;
    export const type = new ProtocolRequestType<BlockAnnotationParams, BlockAnnotation | null, BlockAnnotation, void, void>(method);
    export type HandlerSignature = RequestHandler<BlockAnnotationParams, BlockAnnotation | null, void>;
    // export const capabilities = CM.create('textDocument.inlayHint', 'inlayHintProvider');
}

export function activate(context: vscode.ExtensionContext) {
    context.subscriptions.push(vscode.commands.registerCommand("typescript-go.restart", () => {
        client.restart();
    }));

    const output = vscode.window.createOutputChannel("typescript-go", "log");

    const traceOutput = vscode.window.createOutputChannel("typescript-go (LSP)");

    const exe = context.asAbsolutePath(
        path.join("../", "built", "local", `tsgo${process.platform === "win32" ? ".exe" : ""}`),
    );

    output.appendLine(`Resolved to ${exe}`);

    const config = vscode.workspace.getConfiguration("typescript-go");

    // Get pprofDir
    const pprofDir = config.get<string>("pprofDir");
    const pprofArgs = pprofDir ? ["-pprofDir", pprofDir] : [];

    const serverOptions: ServerOptions = {
        run: {
            command: exe,
            args: ["lsp", ...pprofArgs],
            transport: TransportKind.stdio,
        },
        debug: {
            command: exe,
            args: ["lsp", ...pprofArgs],
            transport: TransportKind.stdio,
        },
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [
            { scheme: "file", language: "typescript" },
            { scheme: "file", language: "typescriptreact" },
            { scheme: "file", language: "javascript" },
            { scheme: "file", language: "javascriptreact" },
            { scheme: "untitled", language: "typescript" },
            { scheme: "untitled", language: "typescriptreact" },
            { scheme: "untitled", language: "javascript" },
            { scheme: "untitled", language: "javascriptreact" },
        ],
        markdown: {
            isTrusted: true,
            supportHtml: true,
        },
        outputChannel: output,
        traceOutputChannel: traceOutput,
        diagnosticPullOptions: {
            onChange: true,
            onSave: true,
            onTabs: true,
            match(documentSelector, resource) {
                // This function is called when diagnostics are requested but
                // only the URI itself is known (e.g. open but not yet focused tabs),
                // so will not be present in vscode.workspace.textDocuments.
                // See if this file matches without consulting vscode.languages.match
                // (which requires a TextDocument).

                const language = getLanguageForUri(resource);

                for (const selector of documentSelector) {
                    if (typeof selector === "string") {
                        if (selector === language) {
                            return true;
                        }
                        continue;
                    }
                    if (NotebookDocumentFilter.is(selector)) {
                        continue;
                    }
                    if (TextDocumentFilter.is(selector)) {
                        if (selector.language !== undefined && selector.language !== language) {
                            continue;
                        }

                        if (selector.scheme !== undefined && selector.scheme !== resource.scheme) {
                            continue;
                        }

                        if (selector.pattern !== undefined) {
                            // VS Code's glob matcher is not available via the API;
                            // see: https://github.com/microsoft/vscode/issues/237304
                            // But, we're only called on selectors passed above, so just ignore this for now.
                            throw new Error("Not implemented");
                        }

                        return true;
                    }
                }

                return false;
            },
        },
    };

    client = new LanguageClient(
        "typescript-go",
        "typescript-go-lsp",
        serverOptions,
        clientOptions,
        true,
    );

    output.appendLine(`Starting language server...`);
    client.start();

    context.subscriptions.push(
        vscode.commands.registerTextEditorCommand("typescript-go.hlsAnnotate", (textEditor: TextEditor, edit: TextEditorEdit, ...args) => {
            output.appendLine(`ANNOTATE ${textEditor.document.uri} ${args}`);
            const params: BlockAnnotationParams = {
                textDocument: client.code2ProtocolConverter.asTextDocumentIdentifier(textEditor.document),
                method: args[0],
                block: args[1],
            };
            client.sendRequest(BlockAnnotationRequest.type, params).then(block => {
                output.appendLine(util.inspect(block, { depth: null }));
                if (block) makePanel(block);
            });
        }),
    );
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

function getLanguageForUri(uri: vscode.Uri): string | undefined {
    const ext = path.posix.extname(uri.path);
    switch (ext) {
        case ".ts":
        case ".mts":
        case ".cts":
            return "typescript";
        case ".js":
        case ".mjs":
        case ".cjs":
            return "javascript";
        case ".tsx":
            return "typescriptreact";
        case ".jsx":
            return "javascriptreact";
        default:
            return undefined;
    }
}

const annotationDecorationType = vscode.window.createTextEditorDecorationType({
    after: {
        margin: "0 0 0 -30px", // Negative left margin to "pull" into the gutter area
        color: "rgba(4, 136, 4, 0.7)",
    },
    rangeBehavior: vscode.DecorationRangeBehavior.OpenOpen,
});

function annotateEditor(editor: vscode.TextEditor) {
    const decorations: vscode.DecorationOptions[] = [];

    for (let line = 3; line < editor.document.lineCount; line++) {
        const lineText = editor.document.lineAt(line).text;

        const blameInfo = parseWithFakeBoldAndUnderline("hi");

        decorations.push({
            range: new vscode.Range(line, 0, line, 0),
            renderOptions: {
                after: {
                    contentText: blameInfo,
                    border: "0 2px 0 0",
                    borderColor: "blue",
                    width: "100px",
                },
            },
        });
    }

    editor.setDecorations(annotationDecorationType, decorations);
}

function toUnicodeBold(text: string): string {
    const A = 0x1d400;
    const a = 0x1d41a;
    const zero = 0x1d7ce;

    return [...text].map(c => {
        if (/[A-Z]/.test(c)) return String.fromCodePoint(A + c.charCodeAt(0) - 65);
        if (/[a-z]/.test(c)) return String.fromCodePoint(a + c.charCodeAt(0) - 97);
        if (/[0-9]/.test(c)) return String.fromCodePoint(zero + c.charCodeAt(0) - 48);
        return c;
    }).join("");
}

function toUnicodeUnderlined(text: string): string {
    const combiningLowLine = "\u0332"; // Unicode combining low line (U+0332)
    return [...text].map(c => c + combiningLowLine).join("");
}

function parseWithFakeBoldAndUnderline(input: string): string {
    return input.replace(/\*\*(.+?)\*\*/g, (_, boldText) => toUnicodeBold(boldText))
        .replace(/__(.+?)__/g, (_, underlinedText) => toUnicodeUnderlined(underlinedText));
}

const GOLDEN_LINE_HEIGHT_RATIO = (process.platform == "darwin") ? 1.5 : 1.35;

function makePanel(block: BlockAnnotation) {
    const panel = vscode.window.createWebviewPanel(
        "exampleWebview", // Identifies the type of the webview (used for internal tracking)
        "Code Explanation", // Title of the webview panel
        vscode.ViewColumn.Beside, // Position the webview beside the code editor
        {
            enableScripts: true, // Allow JavaScript in the webview
            retainContextWhenHidden: true, // Retain context when the webview is hidden
        },
    );

    const config = vscode.workspace.getConfiguration("editor");
    const fontSize = config.get<number>("fontSize") || 14;
    const fontFamily = config.get<string>("fontFamily") || "'Fira Code', 'Source Code Pro', monospace";
    const lineHeight = Math.round(fontSize * GOLDEN_LINE_HEIGHT_RATIO);
    // var lines = Array.from({ length: 30 }, (_, i) => `<div><span style="color: #6E7681";>${1 + i}</span> a = <span style="color: #4FC1FF;">a + 1</span>;</div>`).join("\n");
    // lines += vscode.workspace.getConfiguration("workbench").get("colorTheme");
    var lines = Array.from({ length: block.position.line + 2 }, (_, i) => "<br/>").join("\n");
    lines += block.html;
    panel.webview.html = `
        <!DOCTYPE html>
        <html lang="en">
        <head>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0">
          <title>Code Explanation</title>
          <style>
            body {
              font-family: ${fontFamily};
              font-size: ${fontSize}px;
              line-height: ${lineHeight}px;
              color: #CCCCCC;
            }
            .hls-stage {
              border-top-style: solid;
              border-width: 1px;
              border-color: #CCCCCC;
            }
            .hls-stage-in {
              color: #4FC1FF;
            }
            .hls-stage-out {
              color: #6E7681;
            }
          </style>
        </head>
        <body>
        &nbsp;<br>
        ${lines}
        </body>
        </html>
      `;
}
