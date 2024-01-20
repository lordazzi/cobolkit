import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { MetaModel, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';
import { firstValueFrom } from 'rxjs';
import { environment } from '../../../environments/environment';
import { ICobolkitDrive } from './cobolkit-drive.interface';

@Injectable()
export class CobolkitApi implements ICobolkitDrive {

  constructor(
    private httpClient: HttpClient
  ) { }

  async interpret(copybookContent: string, name: string): Promise<MetaModelStructure[]> {
    const path = `${environment.server}/api/cobolkit/copybook-to-metadata`;
    const body = {
      name, content: copybookContent
    };

    return firstValueFrom(this.httpClient.post<MetaModelStructure[]>(path, body));
  }

  async generatePositionalString(metaData: MetaModelStructure[], metaModel?: MetaModel): Promise<string> {
    const path = `${environment.server}/api/cobolkit/ecma-to-cobol`;
    const body = {
      metaData, metaModel
    };

    return firstValueFrom(this.httpClient.post<string>(path, body));
  }

  async generateMetaData(metaData: MetaModelStructure[], positionalString?: string): Promise<MetaModel> {
    const path = `${environment.server}/api/cobolkit/cobol-to-ecma`;
    const body = {
      metaData, positionalString
    };

    return firstValueFrom(this.httpClient.post<MetaModel>(path, body));
  }
}
