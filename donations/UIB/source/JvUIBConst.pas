{******************************************************************************}
{                        UNIFIED INTERBASE (UIB)                               }
{                                                                              }
{ Project JEDI Code Library (JCL)                                              }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License Version  }
{ 1.0 (the "License"); you may not use this file except in compliance with the }
{ License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is JvUIBLib.pas.                                           }
{                                                                              }
{ The Initial Developer of the Original Code is documented in the accompanying }
{ help file JCL.chm. Portions created by these individuals are Copyright (C)   }
{ 2000 of these individuals.                                                   }
{                                                                              }
{ UIB Constants                                                                }
{                                                                              }
{ Unit owner:    Henri Gourvest                                                }
{ Last modified: Mar 16, 2003                                                  }
{                                                                              }
{******************************************************************************}
{$I JCL.INC}
{$I JvUIB.inc}
unit JvUIBConst;

interface

{$IFNDEF DELPHI6_UP}
const
  S_OK    = $00000000;
  S_FALSE = $00000001;
{$ENDIF}

type
  // JvUIB Server Commands
  TServerCommand = (
    scGetClassObject,
    scInvokeMethod
  );

ResourceString

{$IFDEF UIBLANG_EN}

  sUIBTrue             = 'True';
  sUIBFalse            = 'False';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Incorrect Database Server version, check compiler options.';
  EUIB_CANTLOADLIB         = 'Can''t load library: %s.';
  EUIB_DBHANDLEALLREADYSET = 'Database handle allready assigned, first disconnect database.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaction not assigned.';
  EUIB_DATABASENOTDEF      = 'Database not assigned.';
  EUIB_QUERYNOTOPEN        = 'Query not open.';
  EUIB_CASTERROR           = 'Cast error.';
  EUIB_UNEXPECTEDERROR     = 'Unexpected error.';
  EUIB_FIELDNUMNOTFOUND    = 'Field num: %d not found.';
  EUIB_FIELDSTRNOTFOUND    = 'Field "%s" not found.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  EUIB_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';


  // ORB Errors
  EJvUIB_CantConnect   = 'Can''t connect to server.';
  EJvUIB_ClassNotFound = 'Class not found.';
  EJvUIB_DataType      = 'Data type error.';
{$ELSE}
  {$IFDEF UIBLANG_FR}
    sUIBTrue             = 'Vrai';
    sUIBFalse            = 'Faux';

    // UIB Errors
    EUIB_INVALIDEIBVERSION   = 'Version de base de données incorrecte, vérifiez les options de compilation.';
    EUIB_CANTLOADLIB         = 'Impossible de charger la DLL: %s.';
    EUIB_DBHANDLEALLREADYSET = 'Le handle de la base de données est déjà défini, déconnectez d''abord la base de données.';
    EUIB_TRANSACTIONNOTDEF   = 'La transaction n''est pas définie';
    EUIB_DATABASENOTDEF      = 'La base de données n''est pas définie.';
    EUIB_QUERYNOTOPEN        = 'La requête n''est pas encore ouverte.';
    EUIB_CASTERROR           = 'Transtypage incorrect.';
    EUIB_UNEXPECTEDERROR     = 'Erreur innatendue.';
    EUIB_FIELDNUMNOTFOUND    = 'Le champ numéro: %d ''esxiste pas.';
    EUIB_FIELDSTRNOTFOUND    = 'Le champ "%s" n''existe pas.';
    EUIB_BLOBFIELDNOTFOUND   = 'Le champ Blob numéro: %d n''existe pas.';
    EUIB_FETCHBLOBNOTSET     = 'La propriété FetchBlob doit être activée pour utiliser cette méthode.';


    // ORB Errors
    EJvUIB_CantConnect   = 'Impossible de se connecter au serveur.';
    EJvUIB_ClassNotFound = 'La Classe n''a pas été trouvée.';
    EJvUIB_DataType      = 'Erreur de type de donnée.';
  {$ENDIF}
{$ENDIF}


implementation

end.
