import { biblioteca } from "./Biblioteca";

biblioteca.agregarLibro("El quijote", "cervante", "1241243241");
biblioteca.agregarLibro("habitos atomicos", "james clear", "123124");

const libro = biblioteca.agregarLibro("1984", "orwell", "1984");

biblioteca.registrarSocio(31882, "lucciano", "curotto");
biblioteca.registrarSocio(38481, "agustin", "vergara");
biblioteca.registrarSocio(32312, "samuel", "olmos");

console.log(libro.titulo, libro.autor, libro.isbn);