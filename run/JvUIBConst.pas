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
{ Last modified: September 21, 2003                                            }
{                                                                              }
{******************************************************************************}

{$I JVCL.INC}
{$I JvUIB.inc}

unit JvUIBConst;

interface

{$IFNDEF DELPHI6_UP}
{$IFNDEF BCB}
const
  S_OK    = $00000000;
  S_FALSE = $00000001;
{$ENDIF BCB}
{$ENDIF DELPHI6_UP}


type
  // JvUIB Server Commands
  TServerCommand = (scGetClassObject, scInvokeMethod);

resourceString
  {$IFDEF UIBLANG_EN}
  sUIBTrue  = 'True';
  sUIBFalse = 'False';

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
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" not found.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob field num: %d not found.';
  EUIB_FETCHBLOBNOTSET     = 'FetchBlob property must be set to use this method.';
  EUIB_INDEXERROR          = 'Index out of bound (%d)';
  EUIB_SIZENAME            = 'Size name too big (%s)';
  EUIB_MUSTBEPREPARED      = 'The query must be prepared first.';

  // ORB Errors
  EJvUIB_CantConnect   = 'Can''t connect to server.';
  EJvUIB_ClassNotFound = 'Class not found.';
  EJvUIB_DataType      = 'Data type error.';
  {$ENDIF UIBLANG_EN}

  {$IFDEF UIBLANG_FR}
  sUIBTrue  = 'Vrai';
  sUIBFalse = 'Faux';

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
  EUIB_PARAMSTRNOTFOUND    = 'Le Parametre "%s" n''existe pas.';
  EUIB_BLOBFIELDNOTFOUND   = 'Le champ Blob numéro: %d n''existe pas.';
  EUIB_FETCHBLOBNOTSET     = 'La propriété FetchBlob doit être activée pour utiliser cette méthode.';
  EUIB_INDEXERROR          = 'Indice de liste hors limites (%d)';
  EUIB_SIZENAME            = 'La taille du nom est trop grande (%s)';
  EUIB_MUSTBEPREPARED      = 'La requete doit d''abord etre préparée.';

  // ORB Errors
  EJvUIB_CantConnect   = 'Impossible de se connecter au serveur.';
  EJvUIB_ClassNotFound = 'La Classe n''a pas été trouvée.';
  EJvUIB_DataType      = 'Erreur de type de donnée.';
  {$ENDIF UIBLANG_FR}

  {$IFDEF UIBLANG_CZ}
  sUIBTrue  = 'Ano';
  sUIBFalse = 'Ne';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Nekorektní verze databázového serveru, zkontrolujte nastavení kompileru.';
  EUIB_CANTLOADLIB         = 'Nelze naèíst knihovnu: %s.';
  EUIB_DBHANDLEALLREADYSET = 'Kanál databáze je již pøipraven, nejdøíve odpojte databázi.';
  EUIB_TRANSACTIONNOTDEF   = 'Transakce není pøiøazena.';
  EUIB_DATABASENOTDEF      = 'Databáze není pøiøazena.';
  EUIB_QUERYNOTOPEN        = 'Dotaz není otevøen.';
  EUIB_CASTERROR           = 'Chyba bsazení.';
  EUIB_UNEXPECTEDERROR     = 'Neznámá chyba.';
  EUIB_FIELDNUMNOTFOUND    = 'Položka èíslo: %d neexistuje.';
  EUIB_FIELDSTRNOTFOUND    = 'Položka "%s" neexistuje.';
  EUIB_PARAMSTRNOTFOUND    = 'Parametr "%s" neexistuje.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob záznam èíslo: %d neexistuje.';
  EUIB_FETCHBLOBNOTSET     = 'Vlastnost FetchBlob musí být nastavena pro použití této metody.';
  EUIB_INDEXERROR          = 'Index je mimo rozsah (%d)';
  EUIB_SIZENAME            = 'Velikost názvu je pøíliš velká (%s)';
  EUIB_MUSTBEPREPARED      = 'Dotaz musí být nejdøíve pøipraven (prepared).';

  // ORB Errors
  EJvUIB_CantConnect   = 'Nelze pøipojit server.';
  EJvUIB_ClassNotFound = 'Tøída neexistuje.';
  EJvUIB_DataType      = 'Chybný typ dat.';
  {$ENDIF UIBLANG_CZ}

  {$IFDEF UIBLANG_DE}
  sUIBTrue  = 'Wahr';
  sUIBFalse = 'Falsch';

  // UIB Errors
  EUIB_INVALIDEIBVERSION   = 'Falsche Datenbankserverversion. Bitte ueberpruefen sie die Compiler Optionen.';
  EUIB_CANTLOADLIB         = 'Kann Bibliothek %s nicht laden.';
  EUIB_DBHANDLEALLREADYSET = 'Datenbank Handle bereits zugewiesen. Bitte erst Verbindung von der Datenbank trennen.';
  EUIB_TRANSACTIONNOTDEF   = 'Transaktion nicht zugewiesen.';
  EUIB_DATABASENOTDEF      = 'Datenbank nicht zugewiesen.';
  EUIB_QUERYNOTOPEN        = 'Abfrage nicht geoeffnet.';
  EUIB_CASTERROR           = 'Fehler bei Typumwandlung.';
  EUIB_UNEXPECTEDERROR     = 'Unerwarteter Fehler.';
  EUIB_FIELDNUMNOTFOUND    = 'Feld Nummer: %d not found.';
  EUIB_FIELDSTRNOTFOUND    = 'Feld "%s" nicht gefunden.';
  EUIB_PARAMSTRNOTFOUND    = 'Parameter "%s" nicht gefunden.';
  EUIB_BLOBFIELDNOTFOUND   = 'Blob Feld Nummer: %d nicht gefunden.';
  EUIB_FETCHBLOBNOTSET     = 'Die Eigenschaft FetchBlob muss gesetzt sein um diese Methode zu nutzen.';
  EUIB_INDEXERROR          = 'Index ausserhalb des gueltigen Bereichs. (%d)';
  EUIB_SIZENAME            = 'Name ist zu lang. (%s)';
  EUIB_MUSTBEPREPARED      = 'Die Abfrage muss erst vorbereitet werden.';

  // ORB Errors
  EJvUIB_CantConnect   = 'Verbindung zum Server kann nicht aufgebaut werden.';
  EJvUIB_ClassNotFound = 'Klasse nicht gefunden.';
  EJvUIB_DataType      = 'Datentypfehler.';
  {$ENDIF UIBLANG_DE}

implementation

end.
