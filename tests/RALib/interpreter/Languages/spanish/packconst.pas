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

unit packconst;

interface

const
  SOK               = 'OK';
  SCancel           = 'Cancelar';

 { Dataset Editor }
  SDEDatasetDesigner = '&Editor de campos...';

  SDEAddItem          = '&Agregar campos...';
  SDEDeleteItem       = '&Borrar';
  SDESelectAllItem    = '&Seleccionar todo';
  SDENewItem          = '&Nuevo campo...';

  SDEAddFieldsCaption = 'Agregar campos';
  SDEAvailableFields  = 'Campos disponibles';

  SDENewFieldCaption    = 'Nuevo campo';
  SDEFieldProperties    = 'Propiedades del campo';
  SDEFieldNameLabel     = '&Nombre:';
  SDEFieldTypeLabel     = '&Tipo:';
  SDEComponentNameLabel = '&Componente:';
  SDEFieldSizeLabel     = 'Ta&maño:';
  SDEFieldKind          = 'Tipo del campo';
  SDELookupGroup        = 'Definicion de busqueda';
  SDEKeyFieldsLabel     = 'C&ampos clave:';
  SDEDatasetLabel       = 'Da&taset:';
  SDELookupKeysLabel    = 'Claves de &busqueda:';
  SDEResultFieldLabel   = 'Campo de &resultado:';
  SDEFieldKindItems     = '&Datos'#13'&Calculatdo'#13'&Busqueda';
  SDEFieldTypeMustBeSpecified = 'El tipo de campo debe ser especificado';

  SDBGridColEditor    = '&Editor de columnas...';

 { Collection Editor }
  SCEEditCollection     = 'Editando %s';
  SCEAdd                = '&Agregar';
  SCEDelete             = '&Borrar';
  SCEMoveUp             = 'Mover arr&iba';
  SCEMoveDown           = 'Mover aba&jo';
  SCESelectAllItem      = '&Seleccionar todo';

 { Picture Editor }
  SPELoad               = '&Cargar...';
  SPESave               = '&Guardar...';
  SPEClear              = '&Limpiar';
  SPECopy               = 'C&opiar';
  SPEPaste              = '&Pegar';

 { Menu Designer }
  SMDMenuDesigner       = '&Diseñador de menu';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Borrar';
  SMDCreateSubmenuItem  = 'Crear &sumbenu';

implementation

end.
