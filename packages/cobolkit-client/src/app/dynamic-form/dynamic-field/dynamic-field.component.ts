import { Component, ElementRef, EventEmitter, Input, OnChanges, OnInit, Output, SimpleChange, ViewChild } from '@angular/core';
import { CobolDataType, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';
import { DynamicFieldService } from './dynamic-field.service';
import { FieldError } from './field-error.interface';

@Component({
  selector: 'cbl-dynamic-field',
  templateUrl: './dynamic-field.component.html',
  styleUrls: ['./dynamic-field.component.scss'],
  providers: [
    DynamicFieldService
  ]
})
export class DynamicFieldComponent implements OnInit, OnChanges {

  @Input()
  structure?: MetaModelStructure;

  @Input()
  metaValue: string | number = '';

  @Output()
  inputEvent = new EventEmitter<string | number>();

  @ViewChild('dynamicFieldInput')
  dynamicFieldInput?: ElementRef;

  hasBlur = false;
  hasChanged = false;
  isFocused = false;

  fieldValue = '';
  lastValidValue: string | number = '';
  fieldError: FieldError | false = false;

  isAlphabetic = false;
  isCharacter = false;
  isNumber = false;

  constructor(
    private service: DynamicFieldService
  ) { }

  /**
   * Depois da primeira alteração de valor e primeiro blur, o campo se torna 'dirty',
   * de forma que se torna aceitável a exibição de informações de erro relacionadas
   * ao valor entregue.
   */
  get dirty(): boolean {
    return this.hasBlur && this.hasChanged || false;
  }

  ngOnInit(): void {
    if (this.structure) {
      this.isAlphabetic = (this.structure.metatype.type === CobolDataType.ALPHABETIC);
      this.isCharacter = (this.structure.metatype.type === CobolDataType.CHARACTERES);
      this.isNumber = (this.structure.metatype.type === CobolDataType.NUMBER);
    }

    this.updateValue(String(this.metaValue));
  }

  ngOnChanges(changes: { metaValue: SimpleChange }): void {
    const el: HTMLElement | null = this.dynamicFieldInput && this.dynamicFieldInput.nativeElement || null;
    if (!this.isFocused && el) {
      el.innerHTML = this.fieldValue = String(changes.metaValue.currentValue);
      this.fieldError = false;
    }
  }

  onBlur(): void {
    this.hasBlur = true;
    this.isFocused = false;
  }

  onFocus(): void {
    this.isFocused = true;
  }

  updateValue(entryData: string): void {
    this.fieldValue = entryData;

    try {
      this.lastValidValue = this.service.updateValue(this.fieldValue, this.structure);
      this.fieldError = false;
      this.inputEvent.emit(this.lastValidValue);
    } catch (e) {
      if (this.isInstanceOfFieldError(e)) {
        this.fieldError = e;
      }
    }

    this.hasChanged = true;
  }

  private isInstanceOfFieldError(error: unknown): error is FieldError {
    if (error instanceof Object && 'message' in error && typeof error['message'] === 'string') {
      return true;
    }

    return false;
  }
}
