import { MetaModelStructureType } from "./meta-model-structure-type.model";
import { MetaModelStructureOccurs } from "./meta-model-structure-occurs.model";

export class MetaModelStructure {
    name?: string;
    bookName = 'UNKNOWN';
    occurs: MetaModelStructureOccurs | null = null;
    metatype = new MetaModelStructureType();
    attributes: MetaModelStructure[] | null = null;

    constructor(
        public line: number,
        public level: number
    ) { }
}
