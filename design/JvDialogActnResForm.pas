{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDialogActnResForm.PAS, released on 2004-04-01.

The Initial Developers of the Original Code are:
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$
{$I jvcl.inc}
{$I vclonly.inc}

unit JvDialogActnResForm;

interface

uses
  SysUtils, Classes, JvDialogActns, StdActns,
  JvActions, ActnList, ImgList, Controls, Forms;

type
  TJvDialogActions = class(TDataModule)
    ImageList1: TImageList;
    ActionList1: TActionList;
    JvBrowseForFolderAction1: TJvBrowseForFolderAction;
    JvSelectDirectoryAction1: TJvSelectDirectoryAction;
    JvConnectNetworkAction1: TJvConnectNetworkAction;
    JvFloppyFormatAction1: TJvFloppyFormatAction;
    JvOrganizeFavoritesAction1: TJvOrganizeFavoritesAction;
    JvControlPanelAction1: TJvControlPanelAction;
    JvOpenFileAction1: TJvOpenFileAction;
    JvSaveFileAction1: TJvSaveFileAction;
    JvPageSetupAction1: TJvPageSetupAction;
    JvPageSetupTitledAction1: TJvPageSetupTitledAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
