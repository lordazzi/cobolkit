/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
'use strict';

// Allow for running under nodejs/requirejs in tests
const _monaco: typeof monaco = (typeof monaco === 'undefined' ? (<any>self).monaco : monaco);
export const defineCobol = () => {

    monaco.languages.register({
        id: 'copybook',
        extensions: ['.cpy'],
        aliases: ['copybook', 'cobol']
    });

    // Register a tokens provider for the language
    monaco.languages.setLanguageConfiguration('copybook', {
        comments: {
            lineComment: '*',
        },
        brackets: undefined,
        wordPattern: undefined,
        indentationRules: undefined,
        onEnterRules: undefined,
        autoClosingPairs: undefined,
        surroundingPairs: undefined,
        folding: undefined,
    });

    monaco.languages.setMonarchTokensProvider('copybook', <any>{
        tokenizer: {
            root: [
                [/\*.*/, 'comment'],
                [/^.{6}/, 'reserved-area'],
                [/\.(\s*)$/, 'close-command'],
                [
                    // tslint:disable-next-line:max-line-length
                    /\s+(PIC|PICTURE|VALUES|VALUE|OCCURS|TIMES|DEPENDING ON|TO|INDEXED BY|BINARY|REDEFINES|USAGE|COMP|COMPUTATIONAL|COMP\-1|COMPUTATIONAL\-1|COMP\-2|COMPUTATIONAL\-2)/,
                    'keyword'
                ],
                [
                    /(X)\(\d+\)|S?9*\(\d+\)(V9\(\d+\)|V9+)?/,
                    'type'
                ],
                [
                    /\s+ZEROES|ZEROS|SPACES|ZERO|SPACE/,
                    'constant'
                ],
                [
                    /\s+\-?(\d+(\.\d+)?)/,
                    'number'
                ],
                [
                    /'.*'|".*"/,
                    'string'
                ]
            ]
        }
    });

    // Define a new theme that contains only rules that match this language
    monaco.editor.defineTheme('cobol', {
        base: 'vs-dark',
        colors: {},
        inherit: true,
        rules: [
            { token: 'close-command', foreground: 'ff0000' },
            { token: 'reserved-area', foreground: '666666' },
            { token: 'teste', foreground: 'ff0000' },
            // {
            //     token: 'teste',
            //     foreground: 'ff0000',
            //     fontStyle: 'bold'
            // },
            // { token: 'custom-error', foreground: 'ff0000', fontStyle: 'bold' },
            // { token: 'custom-notice', foreground: 'FFA500' },
            // { token: 'custom-date', foreground: '008800' },
        ]
    });

    monaco.languages.registerCompletionItemProvider('copybook', {
        provideCompletionItems: () => {
            return [
                {
                    label: 'PIC 9',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: {
                        value: 'PIC 9($0).'
                    },
                    documentation: 'Number declaration statement'
                },
                {
                    label: 'PIC S9',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: {
                        value: 'PIC S9($0).'
                    },
                    documentation: 'Signed number declaration statement'
                },
                {
                    label: 'PIC 9V',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: {
                        value: 'PIC 9($1)V($0).'
                    },
                    documentation: 'Decimal number declaration statement'
                },
                {
                    label: 'PIC S9V',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: {
                        value: 'PIC S9($1)V($0).'
                    },
                    documentation: 'Signed decimal number declaration statement'
                },
                {
                    label: 'PIC X',
                    kind: monaco.languages.CompletionItemKind.Snippet,
                    insertText: {
                        value: 'PIC X($0).'
                    },
                    documentation: 'Characteres declaration statement'
                }
            ];
        }
    });
};
