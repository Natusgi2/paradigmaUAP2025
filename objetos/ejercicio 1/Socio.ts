import { Libro } from "./Libro";

class Prestamo {
    constructor(public libro: Libro, public vencimiento: Date) {

    }
}

type Duracion = number; // Define el tipo de duración como número o cadena

export class Socio{
    private librosRetirados: Libro[] = [];
    private vencimientoprestamo: Date[] = [];
    private prestamos: Prestamo[] = [];

    constructor(
        private _id : number,
        private _nombre : string,
        private _apellido: string,
    ){

    }

    get id(){
        return this._id;
    }
    get nombre(){
        return this._nombre;

    }
    get apellido(){
        return this._apellido;
    }
    get nombrecompleto(){
        return `${this.nombre} ${this.apellido}`;
    }
    retirarLibro(libro: Libro, duracion : Duracion) {
        this.librosRetirados.push(libro);
        const vencimiento = new Date();
        vencimiento.setDate(vencimiento.getDate() + duracion);
        this.prestamos.push(new Prestamo(libro, vencimiento));

    }

    devolverLibro(libro: Libro) {
    const prestamo = this.prestamos.find(p => p.libro === libro);
    if (!prestamo) {
        throw new Error("El libro no fue retirado por este socio");
    }
    const indice = this.prestamos.indexOf(prestamo);
    // Eliminar el prestamo en el indice
    this.prestamos.splice(indice, 1);
    return prestamo; // Retorna el libro devuelto
    }

    tieneprestadolibro(libro: Libro): Prestamo | null {
        return this.prestamos.find(p => p.libro === libro) ?? null;
    }


}