import { CommonModule } from '@angular/common';
import { NgModule } from '@angular/core';
import { CobolkitDriveService } from './cobolkit-drive.service';
import { CobolkitApi } from './cobolkit.api';

@NgModule({
  imports: [
    CommonModule
  ],
  providers: [
    CobolkitApi,
    CobolkitDriveService
  ]
})
export class CobolkitModule { }
