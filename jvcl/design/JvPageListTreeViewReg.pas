{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvPageListTreeViewReg.PAS, released on 2003-01-22.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net] .
Portions created by Peter Thörnqvist are Copyright (C) 2003 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:

-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvPageListTreeViewReg;

interface

uses
  Classes,
  {$IFDEF VCL}
  ImgList,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QImgList,
  {$ENDIF VisualCLX}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf, DesignMenus,
  {$IFDEF VCL}
  VCLEditors
  {$ENDIF VCL}
  {$ELSE}
  DsgnIntf, Menus
  {$ENDIF COMPILER6_UP}
  ;

procedure Register;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvPageListTreeViewReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvPageListTreeViewReg.dcr}
{$ENDIF LINUX}

implementation
uses
  {$IFDEF VCL}
  ComCtrls,
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  QComCtrls,
  {$ENDIF VisualCLX}
  JvNavigationPane, JvPageList, JvPageListTreeView, 
  JvDsgnConsts, JvPageListEditors, JvNavPaneEditors,
  JvTreeItemsEditorForm, JvPageLinkEditorForm, JvPageListEditorForm;


procedure Register;
const
  cItems = 'Items';
  cPageList = 'PageList';
  cActivePage = 'ActivePage';

begin
  RegisterComponents('Jv NavPane',[TJvNavigationPane, TJvNavIconButton, TJvNavPanelButton,
    TJvNavPanelHeader, TJvNavPanelDivider, TJvOutlookSplitter, TJvNavPaneStyleManager, TJvNavPaneToolPanel]);

  RegisterComponents(RsPaletteListComboTree, [TJvSettingsTreeView,
    TJvPageListTreeView, TJvPageList]);

  RegisterPropertyEditor(TypeInfo(TTreeNodes), TCustomTreeView, cItems, TJvTreeItemsProperty);
  RegisterPropertyEditor(TypeInfo(TJvShowDesignCaption), nil, '', TJvShowDesignCaptionProperty);
  RegisterClasses([TJvSettingsTreeView, TJvPageListTreeView, TJvPageList, TJvStandardPage]);
  RegisterComponentEditor(TJvCustomPageList, TJvCustomPageEditor);
  RegisterComponentEditor(TJvCustomPage, TJvCustomPageEditor);
  {$IFNDEF COMPILER6_UP}
  RegisterPropertyEditor(TypeInfo(TComponent), TJvCustomPageListTreeView, cPageList, TJvPageListProperty);
  {$ENDIF COMPILER6_UP}
  RegisterComponentEditor(TCustomTreeView, TJvTreeViewComponentEditor);
  RegisterComponentEditor(TJvCustomPageListTreeView, TJvPageTreeViewComponentEditor);
  // register for the standard TTreeView as well
  //  RegisterComponentEditor(TTreeView, TJvTreeViewComponentEditor);
  RegisterPropertyEditor(TypeInfo(TJvPageLinks),
    TJvCustomPageListTreeView, '', TJvPageLinksProperty);
  RegisterPropertyEditor(TypeInfo(TJvCustomPage),
    TJvCustomPageList, cActivePage, TJvActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvSettingsTreeImages, '', TJvSettingsTreeImagesProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelPage, 'ImageIndex', TJvNavPanePageImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelHeader, 'ImageIndex', TJvNavPanelHeaderImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavPanelButton, 'ImageIndex', TJvNavPanelButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvNavIconButton, 'ImageIndex', TJvNavIconButtonImageIndexProperty);

  //  RegisterPropertyEditor(TypeInfo(Integer), TJvSettingsTreeImages, 'CollapsedIndex', TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(TypeInfo(Integer), TJvSettingsTreeImages, 'ExpandedIndex', TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(TypeInfo(Integer), TJvSettingsTreeImages, 'ImageIndex', TJvSettingsTreeImagesProperty);
  //  RegisterPropertyEditor(TypeInfo(Integer), TJvSettingsTreeImages, 'SelectedIndex', TJvSettingsTreeImagesProperty);
end;

//=== TJvCustomPageEditor ====================================================


end.

