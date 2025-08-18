import { Libro } from "./Libro";
import { Socio } from "./Socio";
export class Biblioteca {
    private inventario : Libro[] = [];
    private socio : Socio[] = [];
    private DURACION : number = 14; // Define el tipo de duración como número o cadena
    

    agregarLibro(titulo: string, autor : string, isbn : string){
        const libro = new Libro(titulo,autor,isbn);
        this.inventario.push(libro);
        return libro;
    }
    
    buscarLibro(isbn: string): Libro | null 
    {
    
        const libroencontrado= this.inventario.find(libro => libro.isbn === isbn);
        if (libroencontrado){
            return libroencontrado;
        //return this.inventario.find(libro => libro.isbn === isbn);
    }
    return null;
}

    registrarSocio(id: number, nombre: string, apellido: string){
        const sociocreado = new Socio (id, nombre, apellido);
        this.socio.push(sociocreado);
        return sociocreado;
    }

    buscarSocio(id: number): Socio | null {
        return this.socio.find(socio => socio.id === id) || null;
    }
    retirarLibro(socioId: number, libroisbn: string): void {

        const socio = this.buscarSocio(socioId);
        const libro = this.buscarLibro(libroisbn);
        if (!socio || !libro) {
            throw new Error("Libro no encontrado");
        }
        for (const socio of this.socio) {
            if (socio.tieneprestadolibro(libro)) {
                throw new Error("El socio ya tiene este libro prestado");
            }
        }

        if (!socio) {
            throw new Error("Socio no encontrado");
        }
        socio.retirarLibro(libro, this.DURACION);
    }
    devolverLibro(socioId: number, libroisbn: string): void {
        const socio = this.buscarSocio(socioId);
        const libro = this.buscarLibro(libroisbn);
        if (!socio || !libro) {
            throw new Error("Libro o socio no encontrado");
        }
       socio.devolverLibro(libro);
    }
    
}
export const biblioteca = new Biblioteca()
