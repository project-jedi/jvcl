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

Description : Language specific constant for Spanish

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
  SNoReportProc = 'Procedure "JvInterpreterRunReportPreview" not found';
  SNoReportProc2 = 'Procedure "JvInterpreterRunReportPreview2" not found';

 {JvInterpreter Error Descriptions}
  JvInterpreterErrors : array [0..47] of
    record
      ID: Integer;
      Description: String;
    end
    = (
      (ID:   0; Description: 'Ok'),
      (ID:   1; Description: 'Error desconocido'),
      (ID:   2; Description: 'Error de interprete interno: %s'),
      (ID:   3; Description: 'Usuario interrumpe'),
      (ID:   4; Description: 'Re-levantar una excepcion solo permitido en manejador de excepciones'),
      (ID:   5; Description: 'Error en unidad ''%s'' en linea %d : %s'),
      (ID:   6; Description: 'Error externo en unidad ''%s'' en linea %d : %s'),
      (ID:   7; Description: 'Acceso denegado a ''%s'''),
      (ID:  31; Description: 'Registro ''%s'' no definido'),

      (ID:  52; Description: 'Desborde de stack'),
      (ID:  53; Description: 'Disconcordancia en tipo'),
      (ID:  55; Description: 'Funcion ''main'' indefinida'),
      (ID:  56; Description: 'Unidad ''%s'' no encontrada'),
      (ID:  57; Description: 'Evento ''%s'' no registrado'),
      (ID:  58; Description: 'Dfm ''%s'' no encontrada'),

      (ID: 101; Description: 'Error al remarcar'),
      (ID: 103; Description: '%s esperado pero %s encontrado'),
      (ID: 104; Description: 'Identificador no declarado ''%s'''),
      (ID: 105; Description: 'Tipo de la expresion debe ser booleano'),
      (ID: 106; Description: 'Tipo de la clase requerido'),
      (ID: 107; Description: ''';'' no se permite antes de else'),
      (ID: 108; Description: 'El tipo de la expresion debe ser entero'),
      (ID: 109; Description: 'El tipo del registro, objeto o clase requerido'),
      (ID: 110; Description: 'Operador o coma no se encontro'),
      (ID: 111; Description: 'Identificador redeclarado: ''%s'''),

      (ID: 171; Description: 'Indice de arreglo fuera de limites'),
      (ID: 172; Description: 'Demasiados limites de arreglo'),
      (ID: 173; Description: 'Insuficientes limites en arreglo'),
      (ID: 174; Description: 'Dimension del arreglo invalida'),
      (ID: 175; Description: 'Rango del arreglo invalido'),
      (ID: 176; Description: 'Tipo de arreglo requerido'),

      (ID: 181; Description: 'Demasiados parametros actuales'),
      (ID: 182; Description: 'Parametros insuficientes'),
      (ID: 183; Description: 'Tipos incompatibles: ''%s'' y ''%s'''),
      (ID: 184; Description: 'Error cargando libreria ''%s'''),
      (ID: 185; Description: 'Tipo invalido de argumento en llamada a la funcion ''%s'''),
      (ID: 186; Description: 'Tipo de resultado invalido en llamada a la funcion ''%s'''),
      (ID: 187; Description: 'No se pudo obtener la direccion del proceso para la funcion ''%s'''),
      (ID: 188; Description: 'Tipo de argumento invalido en llamada a la funcion ''%s'''),
      (ID: 189; Description: 'Tipo de resultado invalido en la llamada a la funcion ''%s'''),
      (ID: 190; Description: 'Convencion de llamada invalida para la funcion function ''%s'''),

      (ID: 201; Description: 'Llamando a ''%s'' fallo: ''%s'''),

      (ID: 301; Description: 'Expresion'),
      (ID: 302; Description: 'Identificador'),
      (ID: 303; Description: 'Declaracion'),
      (ID: 304; Description: 'fin de archivo'),
      (ID: 305; Description: 'Declaracion de clase'),

      (ID: 401; Description: 'Implementacion de unidad no encontrada')
    );

implementation

end.
