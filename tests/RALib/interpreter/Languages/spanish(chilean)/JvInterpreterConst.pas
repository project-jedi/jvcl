{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: RAFDAlignPalette.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s): 

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Description : Language specific constant for Spanish(chilean)

Known Issues:
-----------------------------------------------------------------------------}


{$I JVCL.INC}

unit JvInterpreterConst;

interface

const

{JvInterpreterParser}
 {$IFDEF Delphi}
  StIdSymbols      = ['_', '0'..'9', 'A'..'Z', 'a'..'z'];
  StIdFirstSymbols = ['_', 'A'..'Z', 'a'..'z'];
  StConstSymbols   = ['0'..'9', 'A'..'F', 'a'..'f'];
  StConstSymbols10 = ['0'..'9'];
  StSeparators     = ['(', ')', ',', '.', ';'];
 {$ENDIF Delphi}
 {$IFDEF BCB}
  StIdSymbols      = '_0123456789QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StIdFirstSymbols = '_QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm';
  StConstSymbols   = '0123456789ABCDEFabcdef';
  StConstSymbols10 = '0123456789';
  StSeparators     = '(),.;';
 {$ENDIF BCB}

 {JvInterpreterFm}
  SNoReportProc = 'Procedimiento "JvInterpreterRunReportPreview" no encontrado';
  SNoReportProc2 = 'Procedimiento "JvInterpreterRunReportPreview2" no encontrado';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Aceptar'),
      (ID:   1; Description: 'Eror desconocido'),
      (ID:   2; Description: 'Error interno del interprete: %s'),
      (ID:   3; Description: 'Detenido por el usuario'),
      (ID:   4; Description: 'Re-raising una excepción solamente permitida en el manejador de excepciones'),
      (ID:   5; Description: 'Error en unidad ''%s'' en línea %d : %s'),
      (ID:   6; Description: 'Externo en unidad ''%s'' en línea %d : %s'),
      (ID:   7; Description: 'Acceso denegado a ''%s'''),
      (ID:  31; Description: 'Registro ''%s'' no definido'),

      (ID:  52; Description: 'Rebalse de la pila (Stack overflow)'),
      (ID:  53; Description: 'Error de tipo'),
      (ID:  55; Description: 'Función ''main'' indefinida'),
      (ID:  56; Description: 'Unidad ''%s'' no encontrada'),
      (ID:  57; Description: 'Evento ''%s'' no registrado'),
      (ID:  58; Description: 'Dfm ''%s'' no encontrado'),

      (ID: 101; Description: 'Error en comentario'),
      (ID: 103; Description: 'Se esperaba %s pero se encontró %s'),
      (ID: 104; Description: 'Identificador no declarado ''%s'''),
      (ID: 105; Description: 'Tipo de expresión debe ser boolean'),
      (ID: 106; Description: 'Se requiere tipo de Clase'),
      (ID: 107; Description: ''';'' no permitido antes de else'),
      (ID: 108; Description: 'Tipo de expresión debe ser integer'),
      (ID: 109; Description: 'Se requiere tpo de registro, objeto o clase'),
      (ID: 110; Description: 'Falta operador o punto y coma'),
      (ID: 111; Description: 'Identificador redeclarado: ''%s'''),

      (ID: 171; Description: 'Indice del arreglo fuera de limite'),
      (ID: 172; Description: 'Demasiados límites de arreglos'),
      (ID: 173; Description: 'Insuficientes límites de arreglos'),
      (ID: 174; Description: 'Dimensión del arreglo inválida'),
      (ID: 175; Description: 'Rango del arreglo inválido'),
      (ID: 176; Description: 'Se requiere tipo de arreglo'),

      (ID: 181; Description: 'Demasiados parametros'),
      (ID: 182; Description: 'insuficientes parámetros'),
      (ID: 183; Description: 'Tipos incompatibles: ''%s'' y ''%s'''),
      (ID: 184; Description: 'Error cargando biblioteca ''%s'''),
      (ID: 185; Description: 'Tipo de argumento inválido en llamada a función ''%s'''),
      (ID: 186; Description: 'Tipo de resultado inválido en llamada a función ''%s'''),
      (ID: 187; Description: 'No se puede obtener la dirección del proc para la función ''%s'''),
      (ID: 188; Description: 'Tipo de argumento inválido en llamada a función ''%s'''),
      (ID: 189; Description: 'Tipo de resultado inválido en llamada a función ''%s'''),
      (ID: 190; Description: 'Convención de llamada inválida para función ''%s'''),

      (ID: 201; Description: 'Falló llamada a ''%s'': ''%s'''),

      (ID: 301; Description: 'Expresión'),
      (ID: 302; Description: 'Identificador'),
      (ID: 303; Description: 'Declaración'),
      (ID: 304; Description: 'Fin de archivo'),
      (ID: 305; Description: 'declaración de clase'),

      (ID: 401; Description: 'Implementación de unidad no encontrada')
    );

implementation

end.
