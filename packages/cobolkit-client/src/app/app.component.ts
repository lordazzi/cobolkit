import { Component, ElementRef, Inject, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { MetaModel, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';
import { Subscription } from 'rxjs';
import { Exception } from './domain/exception.interface';
import { MetaDataChangeObservable } from './dynamic-form/meta-data-change.observable';
import { ICobolkitDrive } from './shared/cobolkit/cobolkit-drive.interface';
import { CobolkitDriveService } from './shared/cobolkit/cobolkit-drive.service';

@Component({
  selector: 'cbl-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss'],
})
export class AppComponent implements OnInit, OnDestroy {

  exibindoAviso = !(/semAviso/.test(location.hash));

  readonly INITIAL_BOOK = `
      *---------------------------------------------------------------*
      *  Bem vindo ao CobolKit! Substitua o book de entrada de seu    *
      *  programa para editar sua string posicional com um formulário *
      *---------------------------------------------------------------*

          03 PESSOA.
              05  NOME             PIC X(020) VALUE SPACES.
              05  DATA-NASCIMENTO  PIC X(011) VALUE SPACES.
              05  CPF              PIC X(011) VALUE SPACES.
              05  TELEFONES.
                07  TELEFONE-DDD     PIC 9(002) VALUE ZEROS.
                07  TELEFONE-NUMERO  PIC 9(009) VALUE ZEROS.

  `;

  code = localStorage['code'] || this.INITIAL_BOOK;

  editorOptions = {
    language: 'copybook', theme: 'cobol'
  };

  structures?: MetaModelStructure[];

  currentMetaModel: MetaModel = {};
  currentPositionalString = '';
  private readonly subscriptions = new Subscription();
  errors: Exception[] = [];

  @ViewChild('stringPositionalField')
  stringPositionalField?: ElementRef;

  constructor(
    private metaDataChage$: MetaDataChangeObservable,
    @Inject(CobolkitDriveService)
    private cobolkitDrive: ICobolkitDrive
  ) { }

  ngOnInit(): void {
    this.subscriptions.add(this.metaDataChage$.subscribe(() => {
      if (this.structures) {
        this.generatePositionalString(this.structures);
      }
    }));

    setTimeout(() => this.generateStructures(this.code), 0);
  }

  showInString(error: Exception): void {
    const el = this.getStringPositionalFieldHTMLElement();
    if (el) {
      el.focus();
      el.selectionStart = error.fromPos || error.toPos || 0;
      el.selectionEnd = error.toPos || error.fromPos || 0;
    }
  }

  private getStringPositionalFieldHTMLElement(): HTMLTextAreaElement | null {
    return this.stringPositionalField && this.stringPositionalField.nativeElement || null;
  }

  ngOnDestroy(): void {
    this.subscriptions.unsubscribe();
  }

  get hasMetaModel(): boolean {
    return !!Object.keys(this.currentMetaModel).length;
  }

  closeMessage(): void {
    this.exibindoAviso = false;
  }

  generateStructures(code: string): void {
    if (code) {
      localStorage['code'] = code;
    }

    this.generateStructuresPromised(code)
      .then(() => this.errors = [])
      .catch((error: Exception) => this.errors.push(error));
  }

  generatePositionalString(structures: MetaModelStructure[]): void {
    this.generatePositionalStringPromised(structures)
      .then(stringPositional => {
        this.currentPositionalString = stringPositional;
        this.errors = [];
      })
      .catch((error: Exception) => this.errors.push(error));
  }

  generateMetaData(): void {
    if (!this.structures) {
      return;
    }

    this.generateMetaDataPromised(this.structures)
      .then(metaModel => {
        this.currentMetaModel = metaModel;
        this.errors = [];
      })
      .catch((error: Exception) => this.errors.push(error));
  }

  private async generateStructuresPromised(code: string): Promise<void> {
    this.errors = [];

    try {
      this.structures = await this.cobolkitDrive.interpret(code, 'unamed');
      this.currentMetaModel = await this.cobolkitDrive.generateMetaData(this.structures);
      this.currentPositionalString = await this.cobolkitDrive.generatePositionalString(this.structures, this.currentMetaModel);

      return Promise.resolve();
    } catch (e) {
      return Promise.reject(e);
    }
  }

  private generatePositionalStringPromised(structures: MetaModelStructure[]): Promise<string> {
    return this.cobolkitDrive
      .generatePositionalString(structures, this.currentMetaModel);
  }

  private generateMetaDataPromised(structures: MetaModelStructure[]): Promise<MetaModel> {
    return this.cobolkitDrive
      .generateMetaData(structures, this.currentPositionalString);
  }
}
