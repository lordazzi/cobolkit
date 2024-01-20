import { MetaModel } from "./meta-model.type";

export type MetaValue = number | string | MetaModel | (number | string | MetaModel)[];
