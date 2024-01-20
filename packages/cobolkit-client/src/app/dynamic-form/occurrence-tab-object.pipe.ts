import { Pipe, PipeTransform } from '@angular/core';
import { MetaModelStructureOccurs } from '@cobolkit-reborn/cobol-driver';

@Pipe({
  name: 'occurrenceTabObject'
})
export class OccurrenceTabObjectPipe implements PipeTransform {

  transform(occur: MetaModelStructureOccurs | null): { value: number; name: string }[] {
    const occurrences: { value: number; name: string }[] = [];

    if (occur) {
      const length = occur.minLength || occur.maxLength || 0;
      for (let j = 0; j < length; j++) {
        occurrences.push({
          value: j,
          name: String(j + 1)
        });
      }
    }

    return occurrences;
  }

}
