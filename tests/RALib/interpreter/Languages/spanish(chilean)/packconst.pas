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

unit packconst;

interface

const
  SOK               = 'Aceptar';
  SCancel           = 'Cancelar';

 { Dataset Editor }
  SDEDatasetDesigner = 'Editor de &Campos...';

  SDEAddItem          = '&Agregar Campos...';
  SDEDeleteItem       = '&Eliminar';
  SDESelectAllItem    = 'Se&leccionar Tpdps';
  SDENewItem          = '&Nuevo Campo...';

  SDEAddFieldsCaption = 'Agregar campos';
  SDEAvailableFields  = 'Campos disponibles';

  SDENewFieldCaption    = 'Nuevo campo';
  SDEFieldProperties    = 'Propiedades del campo';
  SDEFieldNameLabel     = '&Nombre:';
  SDEFieldTypeLabel     = '&Tipo:';
  SDEComponentNameLabel = 'C&omponente:';
  SDEFieldSizeLabel     = '&Tamaño:';
  SDEFieldKind          = 'Tipo de Campo';
  SDELookupGroup        = 'Definición Lookup';
  SDEKeyFieldsLabel     = 'Campos &Clave:';
  SDEDatasetLabel       = 'D&ataset:';
  SDELookupKeysLabel    = 'Claves Look&up:';
  SDEResultFieldLabel   = 'Campo &Resultante:';
  SDEFieldKindItems     = '&Dato'#13'&Calculado'#13'&Lookup';
  SDEFieldTypeMustBeSpecified = 'El tipo de campo debe ser especificado';

  SDBGridColEditor    = 'Editor de &Columnas...';

 { Collection Editor }
  SCEEditCollection     = 'Editando %s';
  SCEAdd                = '&Agregar';
  SCEDelete             = '&Eliminar';
  SCEMoveUp             = 'Mover &Arriba';
  SCEMoveDown           = 'Mover Aba&jo';
  SCESelectAllItem      = 'Se&leccionar Todo';

 { Picture Editor }
  SPELoad               = '&Cargar...';
  SPESave               = '&Salvar...';
  SPEClear              = '&Limpiar';
  SPECopy               = 'C&opiar';
  SPEPaste              = '&Pegar';

 { Menu Designer }
  SMDMenuDesigner       = '&Diseñador de Menú';
  SMDInsertItem         = '&Insertar';
  SMDDeleteItem         = '&Eliminar';
  SMDCreateSubmenuItem  = 'Crear &Submenú';

implementation

end.
