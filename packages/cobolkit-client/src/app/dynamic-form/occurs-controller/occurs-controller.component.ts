import { Component, Input } from '@angular/core';
import { CobolDataType, MetaModel, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';
import { MetaDataChangeObservable } from '../meta-data-change.observable';

@Component({
  selector: 'cbl-occurs-controller',
  templateUrl: './occurs-controller.component.html',
  styleUrls: ['./occurs-controller.component.scss']
})
export class OccursControllerComponent {

  @Input()
  structure?: MetaModelStructure;

  @Input()
  metaValue?: Partial<MetaModel>;

  occursIndex = 0;

  constructor(
    private whenChanged$: MetaDataChangeObservable
  ) { }

  isObject(attribute: MetaModelStructure): boolean {
    return (attribute.metatype.type === CobolDataType.OBJECT);
  }

  isPrimitive(attribute: MetaModelStructure): boolean {
    return [
      CobolDataType.ALPHABETIC, CobolDataType.CHARACTERES, CobolDataType.NUMBER
    ].includes(attribute.metatype.type);
  }

  treatAsOccurs(): boolean {
    return this.structure && !!this.structure.occurs || false;
  }

  inputToContainer(value: string | number, structure: MetaModelStructure): void {
    if (this.metaValue) {
      this.metaValue[structure.name] = value;
      this.whenChanged$.next();
    }
  }

  inputToOccurs(value: string | number, structure: MetaModelStructure): void {
    if (this.metaValue) {
      (this.metaValue[structure.name] as MetaModel)[this.occursIndex] = value;
      this.whenChanged$.next();
    }
  }
}
