export class Libro {
   /* private _titulo:string;
    private _autor : string;
    private _isbn : string;
*/
    constructor(
        private _titulo: string, 
        private _autor:string, 
        private _isbn: string){
        /*this._titulo = titulo;
        this._autor = autor;
        this._isbn = isbn;*/
    }

    get titulo() {return this._titulo}
    get autor() {return this._autor}
    get isbn() {return this._isbn}
}