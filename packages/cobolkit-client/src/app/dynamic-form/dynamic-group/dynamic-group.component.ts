import { Component, Input } from '@angular/core';
import { MetaModel, MetaModelStructure } from '@cobolkit-reborn/cobol-driver';

@Component({
  selector: 'cbl-dynamic-group',
  templateUrl: './dynamic-group.component.html',
  styleUrls: ['./dynamic-group.component.scss']
})
export class DynamicGroupComponent {

  @Input()
  structure?: MetaModelStructure;

  @Input()
  metaValue?: MetaModel;

}
