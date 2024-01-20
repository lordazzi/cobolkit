import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DynamicFieldComponent } from './dynamic-field/dynamic-field.component';
import { DynamicGroupComponent } from './dynamic-group/dynamic-group.component';
import { FormsModule } from '@angular/forms';
import { MetaDataChangeObservable } from './meta-data-change.observable';
import { OccurrenceTabObjectPipe } from './occurrence-tab-object.pipe';
import { OccursControllerComponent } from './occurs-controller/occurs-controller.component';

@NgModule({
  imports: [
    CommonModule,
    FormsModule
  ],
  declarations: [
    DynamicFieldComponent,
    DynamicGroupComponent,
    OccurrenceTabObjectPipe,
    OccursControllerComponent
  ],
  exports: [
    DynamicFieldComponent,
    DynamicGroupComponent,
    OccursControllerComponent
  ],
  providers: [
    MetaDataChangeObservable
  ]
})
export class DynamicFormModule { }
