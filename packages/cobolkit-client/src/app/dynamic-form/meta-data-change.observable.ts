import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';

@Injectable()
export class MetaDataChangeObservable extends Subject<void> {

}
