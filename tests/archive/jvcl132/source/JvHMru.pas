{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvHMru.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvHMru;

{*******************************************************}
{   This unit is an interface to the MRU List (comctl32)}
{   Informations from :                                 }
{      http://www.geocities.com/SiliconValley/4942      }
{*******************************************************}

interface

uses
  Windows, Messages,
  JvFunctions;

type
  {if the MRUF_STRING_LIST flag is specified, list items are stored in the
  registry as string Values. if the MRUF_BINARY_LIST flag is specified, items
  are stored as binary data. For string lists, the comparison function
  should accept two LPCSTR parameters (the two strings to be compared).
  For binary lists, the comparison function should accept three
  parameters - two LPCVOID parameters (the data for the two items) and
  a DWORD specifying the number of bytes to compare.

  The MRUF_DELAYED_SAVE flag determines how often the list order data is saved
  to the registry. Normally it would be saved whenever you add an item to the
  list, but if the MRUF_DELAYED_SAVE flag is specified when the list is
  constructed, the order data is only saved when the list Handle is
  finally freed. You Free the list using the FreeMRUList function. This will Free
  any internal data structures used by the list, as well as flushing the
  unsaved order data if necessary.}
  MruCompareString = function(lpszstring1, lpszstring2: PChar): Integer;
  MruCompareData = function(lpData1, lpData2: Pointer; cbData: DWORD): Integer;

  {cbSize specifies the Size of the structure.
  nMaxItems specifies the maximum number of items allowed in the list.
  dwFlags specifies the  type of list to Create.
  hKey identifies the Handle of the root registry key under which the list information will be stored.
  lpszSubKey points to a null-terminated string specifying the name of the registry subkey.
  lpfnCompare specifies the function that is used when comparing items in the list.}
  TMruList = record
    cbSize: DWORD;
    nMaxItems: DWORD;
    dwFlags: DWORD;
    hKey: HKEY;
    lpszSubKey: PChar;
    case Boolean of
      True:
      (lpfnComparestring: MruComparestring; );
      False:
      (lpfnCompareData: MruCompareData; );
  end;
  PMruList = ^TMruList;

const
  MRUF_STRING_LIST = 0;
  MRUF_BINARY_LIST = 1;
  MRUF_DELAYED_SAVE = 2;

type
  {You open an MRU list, or Create a new list using the CreateMruList function.
  You pass it a structure with information about the list,  and it will return
  a list Handle which you will use in all subsequent functions.}
  TCreateMruList = function(lpCreateInfo: PMruList): THandle; stdcall;
  TFreeMruList = procedure(hList: THandle); stdcall;

  {You can add an item to the list using the AddMruString function or the
  AddMruData function . AddMruString passes the new item in as a string.
  AddMruData passes the item in as a chunk of binary data - you specify a
  Pointer to a buffer and the number of bytes in the buffer.
        if the item being added already exists, it isn't added again - its
  position is just moved  to the front of the list order. if the list is full,
  the least recently used item in the list is replaced with the new item.
  The return Value is a number corresponding to the registry name where the
  item is stored. 0 corresponds with the 'a' Value, 1 corresponds with
  'b', etc. if there was an error adding the item, the function returns -1.}
  TAddMrustring = function(hList: THandle; lpszstring: PChar): Integer; stdcall;
  TAddMruData = function(hList: THandle; lpData: Pointer; cbData: DWORD): Integer; stdcall;

  {  To remove an item from an MRU list, you can use the DelMruString function,
  passing it the position in the list of the item you wish to remove (0 specifies
  the most recently used item - this is not the same as the return Value
  from the AddMRUxxx functions). DelMruString can also be used for removing
  items from binary lists - there isn't a corresponding DelMruData function,
  although you could easily #define an alias for the sake of consistency.
     Unfortunately, that isn't the end of the story. Due to a bug in the DelMruString
  implementation, if you attempt to add a new item to the list after removing
  an item, you may end up corrupting memory or causing your Application to crash.
  To  work around this, you should always close the list and
  reopen it again before you attempt any other operations on
  the list.
     DelMruString returns TRUE if the item was removed
  successfully. if the specified position is out of range, the
  return Value is FALSE.}
  TDelMruString = function(hList: THandle; nItemPos: Integer): Boolean; stdcall;

  {You can enumerate through the items in a list with the
  EnumMruList function. nItemPos is the position in the list of the item
  you wish to retrieve (0 being the most recently used item).
  lpBuffer points to a buffer where the item will be stored.
  nBufferSize specifies the buffer Size (for string lists the Size is in
  characters; for binary lists it's in bytes).
        The return Value specifies how many bytes were copied to the buffer in
  the case  of a binary list (this may be less than the full Size of the item
  if the buffer is too small). For string lists, the return Value is the full
  Length of the string being retrieved (which may be longer than the
  actual string returned if the buffer is too small). if you enumerate past
  the end of the list, the function returns -1.
        You can also determine the exact number of items in a list by specifying
  a NULL buffer Pointer, or by specifiying a negative Value for nItemPos.
  The return Value is then the number of items in the list.}
  TEnumMruList = function(hList: THandle; nItemPos: Integer; lpBuffer: Pointer; nBufferSize: DWord): Integer; stdcall;

  {if you need to search for a particular item in the list, you should use the
  functions FindMruString and FindMruData. Much like the AddMRUxxx functions,
  FindMruSstring specifies the item as a string and FindMruData passes it in as
  binary data in a buffer.
        The return Value is the position in the list order, 0 being the most
  recently used item. You can also determine the registry Value where the item
  is stored by specifying a non-null Value for the lpdwRegNum Pointer.
  It will be filled in with a  number corresponding to the item's registry name
  (this is  the same as the Value returned by the AddMRUxxx functions).
        if the item could not be found, or there was some other error, the
  functions will return -1.}
  TFindMruString = function(hList: THandle; lpszstring: PChar; lpRegNum: Pinteger): Integer; stdcall;
  TFindMruData = function(hList: THandle; lpData: Pointer; cbData: DWORD; lpRegNum: Pinteger): Integer; stdcall;

var
  CreateMruList: TCreateMruList;
  FreeMruList: TFreeMruList;
  AddMruString: TAddMrustring;
  AddMruData: TAddMruData;
  DelMruString: TDelMruString;
  EnumMruList: TEnumMruList;
  FindMruString: TFindMruString;
  FindMruData: TFindMruData;

implementation

var
  hDll: THandle;

const
  DllName = 'COMCTL32.DLL';

initialization
  hDll := LoadLibrary(DllName);
  if hDll <> 0 then
  begin
    CreateMruList := GetProcAddress(hDll, PChar(151));
    FreeMruList := GetProcAddress(hDll, PChar(152));
    AddMruString := GetProcAddress(hDll, PChar(153));
    AddMruData := GetProcAddress(hDll, PChar(167));
    DelMruString := GetProcAddress(hDll, PChar(156));
    EnumMruList := GetProcAddress(hDll, PChar(154));
    FindMruString := GetProcAddress(hDll, PChar(155));
    FindMruData := GetProcAddress(hDll, PChar(169));
  end
  else
    PError('MRU');

finalization
  if hDll <> 0 then
    FreeLibrary(hDll);

end.
