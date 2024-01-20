import { Injectable } from '@angular/core';
import { CobolToEcmaService, CopybookInterpreter, EcmaToCobolService, MetaModel, MetaModelStructure, MetaValue } from '@cobolkit-reborn/cobol-driver';
import { ICobolkitDrive } from './cobolkit-drive.interface';

@Injectable()
export class CobolkitDriveService implements ICobolkitDrive {

  async interpret(copybookContent: string, name: string): Promise<MetaModelStructure[]> {
    const metaData = CopybookInterpreter
      .getInstance()
      .interpret(copybookContent, name);

    return Promise.resolve(metaData);
  }

  async generatePositionalString(metaData: MetaModelStructure[], metaModel?: MetaModel): Promise<string> {
    const positionalString = EcmaToCobolService
      .getInstance()
      .convert(metaData, metaModel);

    return Promise.resolve(JSON.stringify(positionalString));
  }

  async generateMetaData(metaData: MetaModelStructure[], positionalString?: string): Promise<MetaModel> {
    let metaValue: MetaValue | null = null;

    if (positionalString) {
      metaValue = CobolToEcmaService
        .getInstance()
        .convert(metaData, positionalString);
    } else {
      metaValue = CobolToEcmaService
        .getInstance()
        .convert(metaData);
    }

    //  FIXME: por que precisei fazer essa conversão? pendente de analisar
    //  mais profundamente a tipagem que está circulando nesta lógica
    return Promise.resolve(metaValue as MetaModel);
  }

}
