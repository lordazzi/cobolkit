import { defineCobol } from './cobol-highlight/cobol.contribution';

export function monacoOnload(): void {
  defineCobol();
}
