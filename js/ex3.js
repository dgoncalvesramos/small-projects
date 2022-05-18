function is_prime(nombre) {
    if(nombre == 1 || nombre == 2)
        return true;
    else if(nombre%2==0)
        return false;

    racine = Math.sqrt(nombre);

    if(Number.isInteger(racine))
        return false;
    for(i=3;i<racine;i+=2)
        if(nombre%i==0)
            return false
    return true;
}

function print_nombre_premier(first,last){
    while(first<=last){
        if(is_prime(first))
            document.write("<p> Nombre premier trouvé : " + first +"</p>")
        first++
    }
}