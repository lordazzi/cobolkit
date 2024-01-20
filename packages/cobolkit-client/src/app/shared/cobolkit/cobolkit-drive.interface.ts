import { MetaModel, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';

export interface ICobolkitDrive {
  interpret(copybookContent: string, name: string): Promise<MetaModelStructure[]>;
  generatePositionalString(metaData: MetaModelStructure[], metaModel?: MetaModel): Promise<string>;
  generateMetaData(metaData: MetaModelStructure[], positionalString?: string): Promise<MetaModel>;
}
