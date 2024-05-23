import Data.Time
import System.IO()
import Data.List (find)
import Data.Maybe (isNothing)

-- Definición de tipos de datos
data Vehiculo = Vehiculo { placa :: String, registro :: String, entrada :: UTCTime, salida :: Maybe UTCTime } deriving (Show, Eq)


-- Función principal
main :: IO ()
main = do
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    menu []

-- Función del menú
menu :: [Vehiculo] -> IO ()
menu vehiculos = do
    putStrLn "Seleccione una Opción:"
    putStrLn "1. Registrar la entrada de un Vehículo"
    putStrLn "2. Registrar la salida de un Vehículo"
    putStrLn "3. Buscar Vehículo por Matrícula"
    putStrLn "4. Calcular Tiempo de Permanencia de un Vehículo"
    putStrLn "5. Listar Vehículos"
    putStrLn "6. Salir"

    opcion <- getLine

    case opcion of
        "1" -> do
            vehiculo <- registrarEntrada
            menu (vehiculo:vehiculos)
        "2" -> do
            vehiculosActualizados <- registrarSalida vehiculos
            menu vehiculosActualizados
        "3" -> do
            buscarPorMatricula vehiculos
            menu vehiculos
        "4" -> do
            calcularTiempoPermanencia vehiculos
            menu vehiculos
        "5" -> do
            vehiculosEnArchivo <- cargarVehiculosDesdeArchivo "vehiculos.csv"
            listarVehiculos vehiculosEnArchivo
            menu vehiculos
        "6" -> putStrLn "Saliendo del programa."
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            menu vehiculos

-- Función para guardar vehículos en un archivo CSV
guardarVehiculosEnArchivo :: FilePath -> [Vehiculo] -> IO ()
guardarVehiculosEnArchivo archivo vehiculos = do
    let lineas = map formatVehiculo vehiculos
    writeFile archivo (unlines lineas)

-- Función para formatear un vehículo como una línea de texto en formato CSV
formatVehiculo :: Vehiculo -> String
formatVehiculo vehiculo =
    placa vehiculo ++ "," ++
    registro vehiculo ++ "," ++
    show (entrada vehiculo) ++ "," ++
    maybe "Nothing" show (salida vehiculo)

-- Función para agregar un vehículo al final de un archivo CSV
appendVehiculoEnArchivo :: FilePath -> Vehiculo -> IO ()
appendVehiculoEnArchivo archivo vehiculo = appendFile archivo (formatVehiculo vehiculo ++ "\n")

-- Función para registrar la entrada de un vehículo y guardar el registro en el archivo CSV
registrarEntrada :: IO Vehiculo
registrarEntrada = do
    putStrLn "Ingrese la matrícula del vehículo que entra:"
    placa <- getLine
    horaActual <- getCurrentTime
    let registro = "Registro de entrada"
    let vehiculo = Vehiculo placa registro horaActual Nothing
    appendVehiculoEnArchivo "vehiculos.csv" vehiculo
    return vehiculo

-- Función para actualizar la salida de un vehículo y guardar el registro en el archivo CSV
actualizarSalidaYGuardar :: String -> UTCTime -> [Vehiculo] -> Maybe [Vehiculo]
actualizarSalidaYGuardar _ _ [] = Nothing
actualizarSalidaYGuardar placaSalida salidaTime (vehiculo:resto)
    | placa vehiculo == placaSalida && isNothing (salida vehiculo) = Just $ Vehiculo (placa vehiculo) (registro vehiculo) (entrada vehiculo) (Just salidaTime) : resto
    | otherwise = fmap (vehiculo :) (actualizarSalidaYGuardar placaSalida salidaTime resto)

-- Función para registrar la salida de un vehículo y guardar el registro en el archivo CSV
registrarSalida :: [Vehiculo] -> IO [Vehiculo]
registrarSalida vehiculosOriginales = do
    putStrLn "Ingrese la matrícula del vehículo que sale:"
    placa <- getLine
    horaActual <- getCurrentTime
    let vehiculosActualizados = actualizarSalida placa horaActual vehiculosOriginales
    case vehiculosActualizados of
        Just vehiculos -> do
            putStrLn $ "Vehículo con matrícula " ++ placa ++ " ha salido."
            guardarVehiculosEnArchivo "vehiculos.csv" vehiculos
            return vehiculos
        Nothing -> do
            putStrLn "Vehículo no encontrado."
            return vehiculosOriginales


-- Función para actualizar la salida de un vehículo
actualizarSalida :: String -> UTCTime -> [Vehiculo] -> Maybe [Vehiculo]
actualizarSalida _ _ [] = Nothing
actualizarSalida placaSalida salidaTime (vehiculo:resto)
    | placa vehiculo == placaSalida && isNothing (salida vehiculo) = Just $ Vehiculo (placa vehiculo) (registro vehiculo) (entrada vehiculo) (Just salidaTime) : resto
    | otherwise = fmap (vehiculo :) (actualizarSalida placaSalida salidaTime resto)

-- Función para buscar un vehículo por matrícula
buscarPorMatricula :: [Vehiculo] -> IO ()
buscarPorMatricula vehiculos = do
    putStrLn "Ingrese la matrícula del vehículo que desea buscar:"
    matricula <- getLine
    case find (\vehiculo -> placa vehiculo == matricula) vehiculos of
        Just vehiculo -> putStrLn $ show vehiculo
        Nothing -> putStrLn "Vehículo no encontrado."

-- Función para calcular el tiempo de permanencia de un vehículo
calcularTiempoPermanencia :: [Vehiculo] -> IO ()
calcularTiempoPermanencia vehiculos = do
    putStrLn "Ingrese la matrícula del vehículo cuyo tiempo de permanencia desea calcular:"
    matricula <- getLine
    horaActual <- getCurrentTime
    case find (\vehiculo -> placa vehiculo == matricula) vehiculos of
        Just vehiculo -> case salida vehiculo of
            Just salidaTime -> do
                let diferenciaTiempo = diffUTCTime horaActual salidaTime
                putStrLn $ "El tiempo de permanencia del vehículo con matrícula " ++ matricula ++ " es " ++ show diferenciaTiempo ++ " segundos."
            Nothing -> putStrLn "El vehículo no ha salido."
        Nothing -> putStrLn "Vehículo no encontrado."

-- Función para listar los vehículos
listarVehiculos :: [Vehiculo] -> IO ()
listarVehiculos vehiculos = do
    putStrLn "Lista de Vehículos:"
    mapM_ (putStrLn . show) vehiculos

-- Función para cargar vehículos desde un archivo CSV
cargarVehiculosDesdeArchivo :: FilePath -> IO [Vehiculo]
cargarVehiculosDesdeArchivo archivo = do
    contenido <- readFile archivo
    let registros = lines contenido
    let vehiculos = map parseVehiculo registros
    return vehiculos

-- Función para parsear un registro de vehículo desde una línea de texto
parseVehiculo :: String -> Vehiculo
parseVehiculo registro =
    let
        elems = split ',' registro
    in
        case elems of
            [placa, registro', entrada', salida'] ->
                let
                    entrada = read entrada' :: UTCTime
                    salida = if salida' == "Nothing" then Nothing else Just (read salida' :: UTCTime)
                in
                    Vehiculo placa registro' entrada salida
            _ -> error "Número inválido de elementos en la lista"



-- Función para dividir una cadena en función de un delimitador
split :: Char -> String -> [String]
split _ [] = [""]
split delim (c:cs)
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
  where
    rest = split delim cs
