{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvThemes.PAS, released on 2003-09-25

The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
All Rights Reserved.

Contributors:

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvThemes;

{$I jvcl.inc}
{$IFDEF JVCLThemesEnabled}
{$I windowsonly.inc}
{$ENDIF JVCLThemesEnabled}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, CommCtrl, Types, SysUtils, Classes, Contnrs,
  {$IFDEF HAS_UNIT_SYSTEM_UITYPES}
  System.UITypes,
  {$ENDIF}
  {$IFDEF JVCLThemesEnabled}
    {$IFDEF COMPILER7_UP}
  Themes,
    {$ELSE}
  ThemeSrv,
    {$ENDIF COMPILER7_UP}
  UxTheme,
  {$ENDIF JVCLThemesEnabled}
  Controls, Forms, Graphics, Buttons;

const
 // Add a message handler to a component that is themed by the ThemeManager but
 // should not be themed.
  CM_DENYSUBCLASSING = CM_BASE + 2000; // from ThemeMgr.pas

type
  TCMDenySubClassing = TMessage;

{$IFDEF JVCLThemesEnabled}

{.$MESSAGE HINT 'A few types are IFDEFed out for Pulsar in order to compile this unit. This needs a review.'}
// ahuser: This is more an internal JVCL unit. All the types and constants wouldn't be necessary
//         if we didn't support Delphi 6 with the ThemeManagerD6.

// type name redirection
type
  TThemedElement = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedElement; {$EXTERNALSYM TThemedElement}
  TThemedButton = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedButton; {$EXTERNALSYM TThemedButton}
  TThemedClock = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedClock; {$EXTERNALSYM TThemedClock}
  TThemedComboBox = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedComboBox; {$EXTERNALSYM TThemedComboBox}
  TThemedEdit = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedEdit; {$EXTERNALSYM TThemedEdit}
  TThemedExplorerBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedExplorerBar; {$EXTERNALSYM TThemedExplorerBar}
  TThemedHeader = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedHeader; {$EXTERNALSYM TThemedHeader}
  TThemedListview = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedListview; {$EXTERNALSYM TThemedListview}
  TThemedMenu = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedMenu; {$EXTERNALSYM TThemedMenu}
  TThemedPage = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedPage; {$EXTERNALSYM TThemedPage}
  TThemedProgress = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedProgress; {$EXTERNALSYM TThemedProgress}
  TThemedRebar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedRebar; {$EXTERNALSYM TThemedRebar}
  TThemedScrollBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedScrollBar; {$EXTERNALSYM TThemedScrollBar}
  TThemedSpin = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedSpin; {$EXTERNALSYM TThemedSpin}
  TThemedStartPanel = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedStartPanel; {$EXTERNALSYM TThemedStartPanel}
  TThemedStatus = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedStatus; {$EXTERNALSYM TThemedStatus}
  TThemedTab = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedTab; {$EXTERNALSYM TThemedTab}
  TThemedTaskBand = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedTaskBand; {$EXTERNALSYM TThemedTaskBand}
  TThemedTaskBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedTaskBar; {$EXTERNALSYM TThemedTaskBar}
  TThemedToolBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedToolBar; {$EXTERNALSYM TThemedToolBar}
  TThemedToolTip = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedToolTip; {$EXTERNALSYM TThemedToolTip}
  TThemedTrackBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedTrackBar; {$EXTERNALSYM TThemedTrackBar}
  TThemedTrayNotify = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedTrayNotify; {$EXTERNALSYM TThemedTrayNotify}
  TThemedTreeview = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedTreeview; {$EXTERNALSYM TThemedTreeview}
  TThemedWindow = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedWindow; {$EXTERNALSYM TThemedWindow}
  TThemeData = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemeData; {$EXTERNALSYM TThemeData}

  PThemedElementDetails = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.PThemedElementDetails; {$EXTERNALSYM PThemedElementDetails}
  TThemedElementDetails = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemedElementDetails; {$EXTERNALSYM TThemedElementDetails}
  {$IFDEF COMPILER16_UP}
  TThemeServices = Themes.TCustomStyleServices; {$EXTERNALSYM TThemeServices}
  {$ELSE}
  TThemeServices = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.TThemeServices; {$EXTERNALSYM TThemeServices}
  {$ENDIF COMPILER16_UP}

// enumerations as constants

// TThemedElement
const
  teButton = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teButton; {$EXTERNALSYM teButton}
  teClock = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teClock; {$EXTERNALSYM teClock}
  teComboBox = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teComboBox; {$EXTERNALSYM teComboBox}
  teEdit = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEdit; {$EXTERNALSYM teEdit}
  teExplorerBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teExplorerBar; {$EXTERNALSYM teExplorerBar}
  teHeader = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teHeader; {$EXTERNALSYM teHeader}
  teListView = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teListView; {$EXTERNALSYM teListView}
  teMenu = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teMenu; {$EXTERNALSYM teMenu}
  tePage = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tePage; {$EXTERNALSYM tePage}
  teProgress = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teProgress; {$EXTERNALSYM teProgress}
  teRebar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teRebar; {$EXTERNALSYM teRebar}
  teScrollBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teScrollBar; {$EXTERNALSYM teScrollBar}
  teSpin = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teSpin; {$EXTERNALSYM teSpin}
  teStartPanel = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teStartPanel; {$EXTERNALSYM teStartPanel}
  teStatus = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teStatus; {$EXTERNALSYM teStatus}
  teTab = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teTab; {$EXTERNALSYM teTab}
  teTaskBand = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teTaskBand; {$EXTERNALSYM teTaskBand}
  teTaskBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teTaskBar; {$EXTERNALSYM teTaskBar}
  teToolBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teToolBar; {$EXTERNALSYM teToolBar}
  teToolTip = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teToolTip; {$EXTERNALSYM teToolTip}
  teTrackBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teTrackBar; {$EXTERNALSYM teTrackBar}
  teTrayNotify = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teTrayNotify; {$EXTERNALSYM teTrayNotify}
  teTreeview = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teTreeview; {$EXTERNALSYM teTreeview}
  teWindow = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teWindow; {$EXTERNALSYM teWindow}

// TThemedButton
const
  tbButtonDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbButtonDontCare; {$EXTERNALSYM tbButtonDontCare}
  tbButtonRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbButtonRoot; {$EXTERNALSYM tbButtonRoot}
  tbPushButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbPushButtonNormal; {$EXTERNALSYM tbPushButtonNormal}
  tbPushButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbPushButtonHot; {$EXTERNALSYM tbPushButtonHot}
  tbPushButtonPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbPushButtonPressed; {$EXTERNALSYM tbPushButtonPressed}
  tbPushButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbPushButtonDisabled; {$EXTERNALSYM tbPushButtonDisabled}
  tbPushButtonDefaulted = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbPushButtonDefaulted; {$EXTERNALSYM tbPushButtonDefaulted}
  tbRadioButtonUncheckedNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonUncheckedNormal; {$EXTERNALSYM tbRadioButtonUncheckedNormal}
  tbRadioButtonUncheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonUncheckedHot; {$EXTERNALSYM tbRadioButtonUncheckedHot}
  tbRadioButtonUncheckedPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonUncheckedPressed; {$EXTERNALSYM tbRadioButtonUncheckedPressed}
  tbRadioButtonUncheckedDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonUncheckedDisabled; {$EXTERNALSYM tbRadioButtonUncheckedDisabled}
  tbRadioButtonCheckedNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonCheckedNormal; {$EXTERNALSYM tbRadioButtonCheckedNormal}
  tbRadioButtonCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonCheckedHot; {$EXTERNALSYM tbRadioButtonCheckedHot}
  tbRadioButtonCheckedPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonCheckedPressed; {$EXTERNALSYM tbRadioButtonCheckedPressed}
  tbRadioButtonCheckedDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbRadioButtonCheckedDisabled; {$EXTERNALSYM tbRadioButtonCheckedDisabled}
  tbCheckBoxUncheckedNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxUncheckedNormal; {$EXTERNALSYM tbCheckBoxUncheckedNormal}
  tbCheckBoxUncheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxUncheckedHot; {$EXTERNALSYM tbCheckBoxUncheckedHot}
  tbCheckBoxUncheckedPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxUncheckedPressed; {$EXTERNALSYM tbCheckBoxUncheckedPressed}
  tbCheckBoxUncheckedDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxUncheckedDisabled; {$EXTERNALSYM tbCheckBoxUncheckedDisabled}
  tbCheckBoxCheckedNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxCheckedNormal; {$EXTERNALSYM tbCheckBoxCheckedNormal}
  tbCheckBoxCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxCheckedHot; {$EXTERNALSYM tbCheckBoxCheckedHot}
  tbCheckBoxCheckedPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxCheckedPressed; {$EXTERNALSYM tbCheckBoxCheckedPressed}
  tbCheckBoxCheckedDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxCheckedDisabled; {$EXTERNALSYM tbCheckBoxCheckedDisabled}
  tbCheckBoxMixedNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxMixedNormal; {$EXTERNALSYM tbCheckBoxMixedNormal}
  tbCheckBoxMixedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxMixedHot; {$EXTERNALSYM tbCheckBoxMixedHot}
  tbCheckBoxMixedPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxMixedPressed; {$EXTERNALSYM tbCheckBoxMixedPressed}
  tbCheckBoxMixedDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbCheckBoxMixedDisabled; {$EXTERNALSYM tbCheckBoxMixedDisabled}
  tbGroupBoxNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbGroupBoxNormal; {$EXTERNALSYM tbGroupBoxNormal}
  tbGroupBoxDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbGroupBoxDisabled; {$EXTERNALSYM tbGroupBoxDisabled}
  tbUserButton = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tbUserButton; {$EXTERNALSYM tbUserButton}

// TThemedClock
const
  tcClockDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcClockDontCare; {$EXTERNALSYM tcClockDontCare}
  tcClockRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcClockRoot; {$EXTERNALSYM tcClockRoot}
  tcTimeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcTimeNormal; {$EXTERNALSYM tcTimeNormal}

// TThemedComboBox
const
  tcComboBoxDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcComboBoxDontCare; {$EXTERNALSYM tcComboBoxDontCare}
  tcComboBoxRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcComboBoxRoot; {$EXTERNALSYM tcComboBoxRoot}
  tcDropDownButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcDropDownButtonNormal; {$EXTERNALSYM tcDropDownButtonNormal}
  tcDropDownButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcDropDownButtonHot; {$EXTERNALSYM tcDropDownButtonHot}
  tcDropDownButtonPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcDropDownButtonPressed; {$EXTERNALSYM tcDropDownButtonPressed}
  tcDropDownButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tcDropDownButtonDisabled; {$EXTERNALSYM tcDropDownButtonDisabled}

// TThemedEdit
const
  teEditDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditDontCare; {$EXTERNALSYM teEditDontCare}
  teEditRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditRoot; {$EXTERNALSYM teEditRoot}
  teEditTextNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextNormal; {$EXTERNALSYM teEditTextNormal}
  teEditTextHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextHot; {$EXTERNALSYM teEditTextHot}
  teEditTextSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextSelected; {$EXTERNALSYM teEditTextSelected}
  teEditTextDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextDisabled; {$EXTERNALSYM teEditTextDisabled}
  teEditTextFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextFocused; {$EXTERNALSYM teEditTextFocused}
  teEditTextReadOnly = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextReadOnly; {$EXTERNALSYM teEditTextReadOnly}
  teEditTextAssist = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditTextAssist; {$EXTERNALSYM teEditTextAssist}
  teEditCaret = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.teEditCaret; {$EXTERNALSYM teEditCaret}

// TThemedExplorerBar
const
  tebExplorerBarDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebExplorerBarDontCare; {$EXTERNALSYM tebExplorerBarDontCare}
  tebExplorerBarRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebExplorerBarRoot; {$EXTERNALSYM tebExplorerBarRoot}
  tebHeaderBackgroundNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderBackgroundNormal; {$EXTERNALSYM tebHeaderBackgroundNormal}
  tebHeaderBackgroundHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderBackgroundHot; {$EXTERNALSYM tebHeaderBackgroundHot}
  tebHeaderBackgroundPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderBackgroundPressed; {$EXTERNALSYM tebHeaderBackgroundPressed}
  tebHeaderCloseNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderCloseNormal; {$EXTERNALSYM tebHeaderCloseNormal}
  tebHeaderCloseHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderCloseHot; {$EXTERNALSYM tebHeaderCloseHot}
  tebHeaderClosePressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderClosePressed; {$EXTERNALSYM tebHeaderClosePressed}
  tebHeaderPinNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderPinNormal; {$EXTERNALSYM tebHeaderPinNormal}
  tebHeaderPinHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderPinHot; {$EXTERNALSYM tebHeaderPinHot}
  tebHeaderPinPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderPinPressed; {$EXTERNALSYM tebHeaderPinPressed}
  tebHeaderPinSelectedNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderPinSelectedNormal; {$EXTERNALSYM tebHeaderPinSelectedNormal}
  tebHeaderPinSelectedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderPinSelectedHot; {$EXTERNALSYM tebHeaderPinSelectedHot}
  tebHeaderPinSelectedPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebHeaderPinSelectedPressed; {$EXTERNALSYM tebHeaderPinSelectedPressed}
  tebIEBarMenuNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebIEBarMenuNormal; {$EXTERNALSYM tebIEBarMenuNormal}
  tebIEBarMenuHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebIEBarMenuHot; {$EXTERNALSYM tebIEBarMenuHot}
  tebIEBarMenuPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebIEBarMenuPressed; {$EXTERNALSYM tebIEBarMenuPressed}
  tebNormalGroupBackground = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupBackground; {$EXTERNALSYM tebNormalGroupBackground}
  tebNormalGroupCollapseNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupCollapseNormal; {$EXTERNALSYM tebNormalGroupCollapseNormal}
  tebNormalGroupCollapseHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupCollapseHot; {$EXTERNALSYM tebNormalGroupCollapseHot}
  tebNormalGroupCollapsePressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupCollapsePressed; {$EXTERNALSYM tebNormalGroupCollapsePressed}
  tebNormalGroupExpandNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupExpandNormal; {$EXTERNALSYM tebNormalGroupExpandNormal}
  tebNormalGroupExpandHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupExpandHot; {$EXTERNALSYM tebNormalGroupExpandHot}
  tebNormalGroupExpandPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupExpandPressed; {$EXTERNALSYM tebNormalGroupExpandPressed}
  tebNormalGroupHead = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebNormalGroupHead; {$EXTERNALSYM tebNormalGroupHead}
  tebSpecialGroupBackground = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupBackground; {$EXTERNALSYM tebSpecialGroupBackground}
  {$IFNDEF COMPILER16_UP}
  tebSpecialGroupCollapseSpecial = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupCollapseSpecial; {$EXTERNALSYM tebSpecialGroupCollapseSpecial}
  {$ENDIF ~COMPILER16_UP}
  tebSpecialGroupCollapseHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupCollapseHot; {$EXTERNALSYM tebSpecialGroupCollapseHot}
  tebSpecialGroupCollapsePressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupCollapsePressed; {$EXTERNALSYM tebSpecialGroupCollapsePressed}
  {$IFNDEF COMPILER16_UP}
  tebSpecialGroupExpandSpecial = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupExpandSpecial; {$EXTERNALSYM tebSpecialGroupExpandSpecial}
  {$ENDIF ~COMPILER16_UP}
  tebSpecialGroupExpandHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupExpandHot; {$EXTERNALSYM tebSpecialGroupExpandHot}
  tebSpecialGroupExpandPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupExpandPressed; {$EXTERNALSYM tebSpecialGroupExpandPressed}
  tebSpecialGroupHead = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tebSpecialGroupHead; {$EXTERNALSYM tebSpecialGroupHead}

// TThemedHeader
const
  thHeaderDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderDontCare; {$EXTERNALSYM thHeaderDontCare}
  thHeaderRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderRoot; {$EXTERNALSYM thHeaderRoot}
  thHeaderItemNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemNormal; {$EXTERNALSYM thHeaderItemNormal}
  thHeaderItemHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemHot; {$EXTERNALSYM thHeaderItemHot}
  thHeaderItemPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemPressed; {$EXTERNALSYM thHeaderItemPressed}
  thHeaderItemLeftNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemLeftNormal; {$EXTERNALSYM thHeaderItemLeftNormal}
  thHeaderItemLeftHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemLeftHot; {$EXTERNALSYM thHeaderItemLeftHot}
  thHeaderItemLeftPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemLeftPressed; {$EXTERNALSYM thHeaderItemLeftPressed}
  thHeaderItemRightNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemRightNormal; {$EXTERNALSYM thHeaderItemRightNormal}
  thHeaderItemRightHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemRightHot; {$EXTERNALSYM thHeaderItemRightHot}
  thHeaderItemRightPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderItemRightPressed; {$EXTERNALSYM thHeaderItemRightPressed}
  thHeaderSortArrowSortedUp = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderSortArrowSortedUp; {$EXTERNALSYM thHeaderSortArrowSortedUp}
  thHeaderSortArrowSortedDown = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.thHeaderSortArrowSortedDown; {$EXTERNALSYM thHeaderSortArrowSortedDown}

// TThemedListview
const
  tlListviewDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListviewDontCare; {$EXTERNALSYM tlListviewDontCare}
  tlListviewRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListviewRoot; {$EXTERNALSYM tlListviewRoot}
  tlListItemNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListItemNormal; {$EXTERNALSYM tlListItemNormal}
  tlListItemHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListItemHot; {$EXTERNALSYM tlListItemHot}
  tlListItemSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListItemSelected; {$EXTERNALSYM tlListItemSelected}
  tlListItemDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListItemDisabled; {$EXTERNALSYM tlListItemDisabled}
  tlListItemSelectedNotFocus = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListItemSelectedNotFocus; {$EXTERNALSYM tlListItemSelectedNotFocus}
  tlListGroup = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListGroup; {$EXTERNALSYM tlListGroup}
  tlListDetail = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListDetail; {$EXTERNALSYM tlListDetail}
  tlListSortDetail = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlListSortDetail; {$EXTERNALSYM tlListSortDetail}
  tlEmptyText = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tlEmptyText; {$EXTERNALSYM tlEmptyText}

// TThemedMenu
const
  tmMenuDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuDontCare; {$EXTERNALSYM tmMenuDontCare}
  tmMenuRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuRoot; {$EXTERNALSYM tmMenuRoot}
  tmMenuItemNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuItemNormal; {$EXTERNALSYM tmMenuItemNormal}
  tmMenuItemSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuItemSelected; {$EXTERNALSYM tmMenuItemSelected}
  tmMenuItemDemoted = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuItemDemoted; {$EXTERNALSYM tmMenuItemDemoted}
  tmMenuDropDown = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuDropDown; {$EXTERNALSYM tmMenuDropDown}
  tmMenuBarItem = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuBarItem; {$EXTERNALSYM tmMenuBarItem}
  tmMenuBarDropDown = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmMenuBarDropDown; {$EXTERNALSYM tmMenuBarDropDown}
  tmChevron = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmChevron; {$EXTERNALSYM tmChevron}
  tmSeparator = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tmSeparator; {$EXTERNALSYM tmSeparator}

// TThemedPage
const
  tpPageDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpPageDontCare; {$EXTERNALSYM tpPageDontCare}
  tpPageRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpPageRoot; {$EXTERNALSYM tpPageRoot}
  tpUpNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpNormal; {$EXTERNALSYM tpUpNormal}
  tpUpHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpHot; {$EXTERNALSYM tpUpHot}
  tpUpPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpPressed; {$EXTERNALSYM tpUpPressed}
  tpUpDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpDisabled; {$EXTERNALSYM tpUpDisabled}
  tpDownNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownNormal; {$EXTERNALSYM tpDownNormal}
  tpDownHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownHot; {$EXTERNALSYM tpDownHot}
  tpDownPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownPressed; {$EXTERNALSYM tpDownPressed}
  tpDownDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownDisabled; {$EXTERNALSYM tpDownDisabled}
  tpUpHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpHorzNormal; {$EXTERNALSYM tpUpHorzNormal}
  tpUpHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpHorzHot; {$EXTERNALSYM tpUpHorzHot}
  tpUpHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpHorzPressed; {$EXTERNALSYM tpUpHorzPressed}
  tpUpHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpUpHorzDisabled; {$EXTERNALSYM tpUpHorzDisabled}
  tpDownHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownHorzNormal; {$EXTERNALSYM tpDownHorzNormal}
  tpDownHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownHorzHot; {$EXTERNALSYM tpDownHorzHot}
  tpDownHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownHorzPressed; {$EXTERNALSYM tpDownHorzPressed}
  tpDownHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpDownHorzDisabled; {$EXTERNALSYM tpDownHorzDisabled}

// TThemedProgress
const
  tpProgressDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpProgressDontCare; {$EXTERNALSYM tpProgressDontCare}
  tpProgressRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpProgressRoot; {$EXTERNALSYM tpProgressRoot}
  tpBar = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpBar; {$EXTERNALSYM tpBar}
  tpBarVert = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpBarVert; {$EXTERNALSYM tpBarVert}
  tpChunk = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpChunk; {$EXTERNALSYM tpChunk}
  tpChunkVert = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tpChunkVert; {$EXTERNALSYM tpChunkVert}

// TThemedRebar
const
  trRebarDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trRebarDontCare; {$EXTERNALSYM trRebarDontCare}
  trRebarRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trRebarRoot; {$EXTERNALSYM trRebarRoot}
  trGripper = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trGripper; {$EXTERNALSYM trGripper}
  trGripperVert = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trGripperVert; {$EXTERNALSYM trGripperVert}
  {$IFNDEF COMPILER16_UP}
  trBandNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trBandNormal; {$EXTERNALSYM trBandNormal}
  trBandHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trBandHot; {$EXTERNALSYM trBandHot}
  trBandPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trBandPressed; {$EXTERNALSYM trBandPressed}
  trBandDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trBandDisabled; {$EXTERNALSYM trBandDisabled}
  trBandChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trBandChecked; {$EXTERNALSYM trBandChecked}
  trBandHotChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trBandHotChecked; {$EXTERNALSYM trBandHotChecked}
  {$ENDIF ~COMPILER16_UP}
  trChevronNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronNormal; {$EXTERNALSYM trChevronNormal}
  trChevronHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronHot; {$EXTERNALSYM trChevronHot}
  trChevronPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronPressed; {$EXTERNALSYM trChevronPressed}
  {$IFNDEF COMPILER16_UP}
  trChevronDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronDisabled; {$EXTERNALSYM trChevronDisabled}
  {$ENDIF ~COMPILER16_UP}
  trChevronVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronVertNormal; {$EXTERNALSYM trChevronVertNormal}
  trChevronVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronVertHot; {$EXTERNALSYM trChevronVertHot}
  trChevronVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronVertPressed; {$EXTERNALSYM trChevronVertPressed}
  {$IFNDEF COMPILER16_UP}
  trChevronVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.trChevronVertDisabled; {$EXTERNALSYM trChevronVertDisabled}
  {$ENDIF ~COMPILER16_UP}

// TThemedScrollBar
const
  tsScrollBarDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsScrollBarDontCare; {$EXTERNALSYM tsScrollBarDontCare}
  tsScrollBarRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsScrollBarRoot; {$EXTERNALSYM tsScrollBarRoot}
  tsArrowBtnUpNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnUpNormal; {$EXTERNALSYM tsArrowBtnUpNormal}
  tsArrowBtnUpHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnUpHot; {$EXTERNALSYM tsArrowBtnUpHot}
  tsArrowBtnUpPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnUpPressed; {$EXTERNALSYM tsArrowBtnUpPressed}
  tsArrowBtnUpDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnUpDisabled; {$EXTERNALSYM tsArrowBtnUpDisabled}
  tsArrowBtnDownNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnDownNormal; {$EXTERNALSYM tsArrowBtnDownNormal}
  tsArrowBtnDownHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnDownHot; {$EXTERNALSYM tsArrowBtnDownHot}
  tsArrowBtnDownPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnDownPressed; {$EXTERNALSYM tsArrowBtnDownPressed}
  tsArrowBtnDownDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnDownDisabled; {$EXTERNALSYM tsArrowBtnDownDisabled}
  tsArrowBtnLeftNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnLeftNormal; {$EXTERNALSYM tsArrowBtnLeftNormal}
  tsArrowBtnLeftHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnLeftHot; {$EXTERNALSYM tsArrowBtnLeftHot}
  tsArrowBtnLeftPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnLeftPressed; {$EXTERNALSYM tsArrowBtnLeftPressed}
  tsArrowBtnLeftDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnLeftDisabled; {$EXTERNALSYM tsArrowBtnLeftDisabled}
  tsArrowBtnRightNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnRightNormal; {$EXTERNALSYM tsArrowBtnRightNormal}
  tsArrowBtnRightHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnRightHot; {$EXTERNALSYM tsArrowBtnRightHot}
  tsArrowBtnRightPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnRightPressed; {$EXTERNALSYM tsArrowBtnRightPressed}
  tsArrowBtnRightDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsArrowBtnRightDisabled; {$EXTERNALSYM tsArrowBtnRightDisabled}
  tsThumbBtnHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnHorzNormal; {$EXTERNALSYM tsThumbBtnHorzNormal}
  tsThumbBtnHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnHorzHot; {$EXTERNALSYM tsThumbBtnHorzHot}
  tsThumbBtnHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnHorzPressed; {$EXTERNALSYM tsThumbBtnHorzPressed}
  tsThumbBtnHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnHorzDisabled; {$EXTERNALSYM tsThumbBtnHorzDisabled}
  tsThumbBtnVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnVertNormal; {$EXTERNALSYM tsThumbBtnVertNormal}
  tsThumbBtnVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnVertHot; {$EXTERNALSYM tsThumbBtnVertHot}
  tsThumbBtnVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnVertPressed; {$EXTERNALSYM tsThumbBtnVertPressed}
  tsThumbBtnVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsThumbBtnVertDisabled; {$EXTERNALSYM tsThumbBtnVertDisabled}
  tsLowerTrackHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackHorzNormal; {$EXTERNALSYM tsLowerTrackHorzNormal}
  tsLowerTrackHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackHorzHot; {$EXTERNALSYM tsLowerTrackHorzHot}
  tsLowerTrackHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackHorzPressed; {$EXTERNALSYM tsLowerTrackHorzPressed}
  tsLowerTrackHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackHorzDisabled; {$EXTERNALSYM tsLowerTrackHorzDisabled}
  tsUpperTrackHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackHorzNormal; {$EXTERNALSYM tsUpperTrackHorzNormal}
  tsUpperTrackHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackHorzHot; {$EXTERNALSYM tsUpperTrackHorzHot}
  tsUpperTrackHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackHorzPressed; {$EXTERNALSYM tsUpperTrackHorzPressed}
  tsUpperTrackHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackHorzDisabled; {$EXTERNALSYM tsUpperTrackHorzDisabled}
  tsLowerTrackVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackVertNormal; {$EXTERNALSYM tsLowerTrackVertNormal}
  tsLowerTrackVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackVertHot; {$EXTERNALSYM tsLowerTrackVertHot}
  tsLowerTrackVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackVertPressed; {$EXTERNALSYM tsLowerTrackVertPressed}
  tsLowerTrackVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsLowerTrackVertDisabled; {$EXTERNALSYM tsLowerTrackVertDisabled}
  tsUpperTrackVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackVertNormal; {$EXTERNALSYM tsUpperTrackVertNormal}
  tsUpperTrackVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackVertHot; {$EXTERNALSYM tsUpperTrackVertHot}
  tsUpperTrackVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackVertPressed; {$EXTERNALSYM tsUpperTrackVertPressed}
  tsUpperTrackVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpperTrackVertDisabled; {$EXTERNALSYM tsUpperTrackVertDisabled}
  tsGripperHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperHorzNormal; {$EXTERNALSYM tsGripperHorzNormal}
  tsGripperHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperHorzHot; {$EXTERNALSYM tsGripperHorzHot}
  tsGripperHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperHorzPressed; {$EXTERNALSYM tsGripperHorzPressed}
  tsGripperHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperHorzDisabled; {$EXTERNALSYM tsGripperHorzDisabled}
  tsGripperVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperVertNormal; {$EXTERNALSYM tsGripperVertNormal}
  tsGripperVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperVertHot; {$EXTERNALSYM tsGripperVertHot}
  tsGripperVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperVertPressed; {$EXTERNALSYM tsGripperVertPressed}
  tsGripperVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperVertDisabled; {$EXTERNALSYM tsGripperVertDisabled}
  tsSizeBoxRightAlign = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsSizeBoxRightAlign; {$EXTERNALSYM tsSizeBoxRightAlign}
  tsSizeBoxLeftAlign = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsSizeBoxLeftAlign; {$EXTERNALSYM tsSizeBoxLeftAlign}

// TThemedSpin
const
  tsSpinDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsSpinDontCare; {$EXTERNALSYM tsSpinDontCare}
  tsSpinRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsSpinRoot; {$EXTERNALSYM tsSpinRoot}
  tsUpNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpNormal; {$EXTERNALSYM tsUpNormal}
  tsUpHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpHot; {$EXTERNALSYM tsUpHot}
  tsUpPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpPressed; {$EXTERNALSYM tsUpPressed}
  tsUpDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpDisabled; {$EXTERNALSYM tsUpDisabled}
  tsDownNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownNormal; {$EXTERNALSYM tsDownNormal}
  tsDownHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownHot; {$EXTERNALSYM tsDownHot}
  tsDownPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownPressed; {$EXTERNALSYM tsDownPressed}
  tsDownDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownDisabled; {$EXTERNALSYM tsDownDisabled}
  tsUpHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpHorzNormal; {$EXTERNALSYM tsUpHorzNormal}
  tsUpHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpHorzHot; {$EXTERNALSYM tsUpHorzHot}
  tsUpHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpHorzPressed; {$EXTERNALSYM tsUpHorzPressed}
  tsUpHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsUpHorzDisabled; {$EXTERNALSYM tsUpHorzDisabled}
  tsDownHorzNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownHorzNormal; {$EXTERNALSYM tsDownHorzNormal}
  tsDownHorzHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownHorzHot; {$EXTERNALSYM tsDownHorzHot}
  tsDownHorzPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownHorzPressed; {$EXTERNALSYM tsDownHorzPressed}
  tsDownHorzDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsDownHorzDisabled; {$EXTERNALSYM tsDownHorzDisabled}

// TThemedStartPanel
const
  tspStartPanelDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspStartPanelDontCare; {$EXTERNALSYM tspStartPanelDontCare}
  tspStartPanelRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspStartPanelRoot; {$EXTERNALSYM tspStartPanelRoot}
  tspUserPane = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspUserPane; {$EXTERNALSYM tspUserPane}
  tspMorePrograms = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspMorePrograms; {$EXTERNALSYM tspMorePrograms}
  tspMoreProgramsArrowNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspMoreProgramsArrowNormal; {$EXTERNALSYM tspMoreProgramsArrowNormal}
  tspMoreProgramsArrowHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspMoreProgramsArrowHot; {$EXTERNALSYM tspMoreProgramsArrowHot}
  tspMoreProgramsArrowPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspMoreProgramsArrowPressed; {$EXTERNALSYM tspMoreProgramsArrowPressed}
  tspProgList = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspProgList; {$EXTERNALSYM tspProgList}
  tspProgListSeparator = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspProgListSeparator; {$EXTERNALSYM tspProgListSeparator}
  tspPlacesList = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspPlacesList; {$EXTERNALSYM tspPlacesList}
  tspPlacesListSeparator = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspPlacesListSeparator; {$EXTERNALSYM tspPlacesListSeparator}
  tspLogOff = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspLogOff; {$EXTERNALSYM tspLogOff}
  tspLogOffButtonsNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspLogOffButtonsNormal; {$EXTERNALSYM tspLogOffButtonsNormal}
  tspLogOffButtonsHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspLogOffButtonsHot; {$EXTERNALSYM tspLogOffButtonsHot}
  tspLogOffButtonsPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspLogOffButtonsPressed; {$EXTERNALSYM tspLogOffButtonsPressed}
  tspUserPicture = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspUserPicture; {$EXTERNALSYM tspUserPicture}
  tspPreview = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tspPreview; {$EXTERNALSYM tspPreview}

// TThemedStatus
const
  tsStatusDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsStatusDontCare; {$EXTERNALSYM tsStatusDontCare}
  tsStatusRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsStatusRoot; {$EXTERNALSYM tsStatusRoot}
  tsPane = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsPane; {$EXTERNALSYM tsPane}
  tsGripperPane = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripperPane; {$EXTERNALSYM tsGripperPane}
  tsGripper = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tsGripper; {$EXTERNALSYM tsGripper}

// TThemedTab
const
  ttTabDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabDontCare; {$EXTERNALSYM ttTabDontCare}
  ttTabRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabRoot; {$EXTERNALSYM ttTabRoot}
  ttTabItemNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemNormal; {$EXTERNALSYM ttTabItemNormal}
  ttTabItemHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemHot; {$EXTERNALSYM ttTabItemHot}
  ttTabItemSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemSelected; {$EXTERNALSYM ttTabItemSelected}
  ttTabItemDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemDisabled; {$EXTERNALSYM ttTabItemDisabled}
  ttTabItemFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemFocused; {$EXTERNALSYM ttTabItemFocused}
  ttTabItemLeftEdgeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemLeftEdgeNormal; {$EXTERNALSYM ttTabItemLeftEdgeNormal}
  ttTabItemLeftEdgeHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemLeftEdgeHot; {$EXTERNALSYM ttTabItemLeftEdgeHot}
  ttTabItemLeftEdgeSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemLeftEdgeSelected; {$EXTERNALSYM ttTabItemLeftEdgeSelected}
  ttTabItemLeftEdgeDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemLeftEdgeDisabled; {$EXTERNALSYM ttTabItemLeftEdgeDisabled}
  ttTabItemLeftEdgeFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemLeftEdgeFocused; {$EXTERNALSYM ttTabItemLeftEdgeFocused}
  ttTabItemRightEdgeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemRightEdgeNormal; {$EXTERNALSYM ttTabItemRightEdgeNormal}
  ttTabItemRightEdgeHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemRightEdgeHot; {$EXTERNALSYM ttTabItemRightEdgeHot}
  ttTabItemRightEdgeSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemRightEdgeSelected; {$EXTERNALSYM ttTabItemRightEdgeSelected}
  ttTabItemRightEdgeDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemRightEdgeDisabled; {$EXTERNALSYM ttTabItemRightEdgeDisabled}
  ttTabItemRightEdgeFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemRightEdgeFocused; {$EXTERNALSYM ttTabItemRightEdgeFocused}
  ttTabItemBothEdgeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemBothEdgeNormal; {$EXTERNALSYM ttTabItemBothEdgeNormal}
  ttTabItemBothEdgeHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemBothEdgeHot; {$EXTERNALSYM ttTabItemBothEdgeHot}
  ttTabItemBothEdgeSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemBothEdgeSelected; {$EXTERNALSYM ttTabItemBothEdgeSelected}
  ttTabItemBothEdgeDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemBothEdgeDisabled; {$EXTERNALSYM ttTabItemBothEdgeDisabled}
  ttTabItemBothEdgeFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTabItemBothEdgeFocused; {$EXTERNALSYM ttTabItemBothEdgeFocused}
  ttTopTabItemNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemNormal; {$EXTERNALSYM ttTopTabItemNormal}
  ttTopTabItemHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemHot; {$EXTERNALSYM ttTopTabItemHot}
  ttTopTabItemSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemSelected; {$EXTERNALSYM ttTopTabItemSelected}
  ttTopTabItemDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemDisabled; {$EXTERNALSYM ttTopTabItemDisabled}
  ttTopTabItemFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemFocused; {$EXTERNALSYM ttTopTabItemFocused}
  ttTopTabItemLeftEdgeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemLeftEdgeNormal; {$EXTERNALSYM ttTopTabItemLeftEdgeNormal}
  ttTopTabItemLeftEdgeHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemLeftEdgeHot; {$EXTERNALSYM ttTopTabItemLeftEdgeHot}
  ttTopTabItemLeftEdgeSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemLeftEdgeSelected; {$EXTERNALSYM ttTopTabItemLeftEdgeSelected}
  ttTopTabItemLeftEdgeDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemLeftEdgeDisabled; {$EXTERNALSYM ttTopTabItemLeftEdgeDisabled}
  ttTopTabItemLeftEdgeFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemLeftEdgeFocused; {$EXTERNALSYM ttTopTabItemLeftEdgeFocused}
  ttTopTabItemRightEdgeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemRightEdgeNormal; {$EXTERNALSYM ttTopTabItemRightEdgeNormal}
  ttTopTabItemRightEdgeHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemRightEdgeHot; {$EXTERNALSYM ttTopTabItemRightEdgeHot}
  ttTopTabItemRightEdgeSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemRightEdgeSelected; {$EXTERNALSYM ttTopTabItemRightEdgeSelected}
  ttTopTabItemRightEdgeDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemRightEdgeDisabled; {$EXTERNALSYM ttTopTabItemRightEdgeDisabled}
  ttTopTabItemRightEdgeFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemRightEdgeFocused; {$EXTERNALSYM ttTopTabItemRightEdgeFocused}
  ttTopTabItemBothEdgeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemBothEdgeNormal; {$EXTERNALSYM ttTopTabItemBothEdgeNormal}
  ttTopTabItemBothEdgeHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemBothEdgeHot; {$EXTERNALSYM ttTopTabItemBothEdgeHot}
  ttTopTabItemBothEdgeSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemBothEdgeSelected; {$EXTERNALSYM ttTopTabItemBothEdgeSelected}
  ttTopTabItemBothEdgeDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemBothEdgeDisabled; {$EXTERNALSYM ttTopTabItemBothEdgeDisabled}
  ttTopTabItemBothEdgeFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTopTabItemBothEdgeFocused; {$EXTERNALSYM ttTopTabItemBothEdgeFocused}
  ttPane = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttPane; {$EXTERNALSYM ttPane}
  ttBody = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttBody; {$EXTERNALSYM ttBody}

// TThemedTaskBand
const
  ttbTaskBandDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTaskBandDontCare; {$EXTERNALSYM ttbTaskBandDontCare}
  ttbTaskBandRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTaskBandRoot; {$EXTERNALSYM ttbTaskBandRoot}
  ttbGroupCount = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbGroupCount; {$EXTERNALSYM ttbGroupCount}
  ttbFlashButton = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbFlashButton; {$EXTERNALSYM ttbFlashButton}
  ttpFlashButtonGroupMenu = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttpFlashButtonGroupMenu; {$EXTERNALSYM ttpFlashButtonGroupMenu}

{$IFNDEF COMPILER16_UP}
// TThemedTaskBar
const
  ttTaskBarDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTaskBarDontCare; {$EXTERNALSYM ttTaskBarDontCare}
  ttTaskBarRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTaskBarRoot; {$EXTERNALSYM ttTaskBarRoot}
  ttbTimeNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTimeNormal; {$EXTERNALSYM ttbTimeNormal}
{$ENDIF ~COMPILER16_UP}

// TThemedToolBar
const
  ttbToolBarDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbToolBarDontCare; {$EXTERNALSYM ttbToolBarDontCare}
  ttbToolBarRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbToolBarRoot; {$EXTERNALSYM ttbToolBarRoot}
  ttbButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbButtonNormal; {$EXTERNALSYM ttbButtonNormal}
  ttbButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbButtonHot; {$EXTERNALSYM ttbButtonHot}
  ttbButtonPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbButtonPressed; {$EXTERNALSYM ttbButtonPressed}
  ttbButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbButtonDisabled; {$EXTERNALSYM ttbButtonDisabled}
  ttbButtonChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbButtonChecked; {$EXTERNALSYM ttbButtonChecked}
  ttbButtonCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbButtonCheckedHot; {$EXTERNALSYM ttbButtonCheckedHot}
  ttbDropDownButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbDropDownButtonNormal; {$EXTERNALSYM ttbDropDownButtonNormal}
  ttbDropDownButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbDropDownButtonHot; {$EXTERNALSYM ttbDropDownButtonHot}
  ttbDropDownButtonPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbDropDownButtonPressed; {$EXTERNALSYM ttbDropDownButtonPressed}
  ttbDropDownButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbDropDownButtonDisabled; {$EXTERNALSYM ttbDropDownButtonDisabled}
  ttbDropDownButtonChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbDropDownButtonChecked; {$EXTERNALSYM ttbDropDownButtonChecked}
  ttbDropDownButtonCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbDropDownButtonCheckedHot; {$EXTERNALSYM ttbDropDownButtonCheckedHot}
  ttbSplitButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonNormal; {$EXTERNALSYM ttbSplitButtonNormal}
  ttbSplitButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonHot; {$EXTERNALSYM ttbSplitButtonHot}
  ttbSplitButtonPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonPressed; {$EXTERNALSYM ttbSplitButtonPressed}
  ttbSplitButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDisabled; {$EXTERNALSYM ttbSplitButtonDisabled}
  ttbSplitButtonChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonChecked; {$EXTERNALSYM ttbSplitButtonChecked}
  ttbSplitButtonCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonCheckedHot; {$EXTERNALSYM ttbSplitButtonCheckedHot}
  ttbSplitButtonDropDownNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDropDownNormal; {$EXTERNALSYM ttbSplitButtonDropDownNormal}
  ttbSplitButtonDropDownHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDropDownHot; {$EXTERNALSYM ttbSplitButtonDropDownHot}
  ttbSplitButtonDropDownPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDropDownPressed; {$EXTERNALSYM ttbSplitButtonDropDownPressed}
  ttbSplitButtonDropDownDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDropDownDisabled; {$EXTERNALSYM ttbSplitButtonDropDownDisabled}
  ttbSplitButtonDropDownChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDropDownChecked; {$EXTERNALSYM ttbSplitButtonDropDownChecked}
  ttbSplitButtonDropDownCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSplitButtonDropDownCheckedHot; {$EXTERNALSYM ttbSplitButtonDropDownCheckedHot}
  ttbSeparatorNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorNormal; {$EXTERNALSYM ttbSeparatorNormal}
  ttbSeparatorHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorHot; {$EXTERNALSYM ttbSeparatorHot}
  ttbSeparatorPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorPressed; {$EXTERNALSYM ttbSeparatorPressed}
  ttbSeparatorDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorDisabled; {$EXTERNALSYM ttbSeparatorDisabled}
  ttbSeparatorChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorChecked; {$EXTERNALSYM ttbSeparatorChecked}
  ttbSeparatorCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorCheckedHot; {$EXTERNALSYM ttbSeparatorCheckedHot}
  ttbSeparatorVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorVertNormal; {$EXTERNALSYM ttbSeparatorVertNormal}
  ttbSeparatorVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorVertHot; {$EXTERNALSYM ttbSeparatorVertHot}
  ttbSeparatorVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorVertPressed; {$EXTERNALSYM ttbSeparatorVertPressed}
  ttbSeparatorVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorVertDisabled; {$EXTERNALSYM ttbSeparatorVertDisabled}
  ttbSeparatorVertChecked = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorVertChecked; {$EXTERNALSYM ttbSeparatorVertChecked}
  ttbSeparatorVertCheckedHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbSeparatorVertCheckedHot; {$EXTERNALSYM ttbSeparatorVertCheckedHot}

// TThemedToolTip
const
  tttToolTipDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttToolTipDontCare; {$EXTERNALSYM tttToolTipDontCare}
  tttToolTipRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttToolTipRoot; {$EXTERNALSYM tttToolTipRoot}
  tttStandardNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttStandardNormal; {$EXTERNALSYM tttStandardNormal}
  tttStandardLink = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttStandardLink; {$EXTERNALSYM tttStandardLink}
  tttStandardTitleNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttStandardTitleNormal; {$EXTERNALSYM tttStandardTitleNormal}
  tttStandardTitleLink = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttStandardTitleLink; {$EXTERNALSYM tttStandardTitleLink}
  tttBaloonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttBaloonNormal; {$EXTERNALSYM tttBaloonNormal}
  tttBaloonLink = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttBaloonLink; {$EXTERNALSYM tttBaloonLink}
  tttBaloonTitleNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttBaloonTitleNormal; {$EXTERNALSYM tttBaloonTitleNormal}
  tttBaloonTitleLink = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttBaloonTitleLink; {$EXTERNALSYM tttBaloonTitleLink}
  tttCloseNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttCloseNormal; {$EXTERNALSYM tttCloseNormal}
  tttCloseHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttCloseHot; {$EXTERNALSYM tttCloseHot}
  tttClosePressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.tttClosePressed; {$EXTERNALSYM tttClosePressed}

// TThemedTrackBar
const
  ttbTrackBarDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTrackBarDontCare; {$EXTERNALSYM ttbTrackBarDontCare}
  ttbTrackBarRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTrackBarRoot; {$EXTERNALSYM ttbTrackBarRoot}
  ttbTrack = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTrack; {$EXTERNALSYM ttbTrack}
  ttbTrackVert = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbTrackVert; {$EXTERNALSYM ttbTrackVert}
  ttbThumbNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbNormal; {$EXTERNALSYM ttbThumbNormal}
  ttbThumbHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbHot; {$EXTERNALSYM ttbThumbHot}
  ttbThumbPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbPressed; {$EXTERNALSYM ttbThumbPressed}
  ttbThumbFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbFocused; {$EXTERNALSYM ttbThumbFocused}
  ttbThumbDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbDisabled; {$EXTERNALSYM ttbThumbDisabled}
  ttbThumbBottomNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbBottomNormal; {$EXTERNALSYM ttbThumbBottomNormal}
  ttbThumbBottomHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbBottomHot; {$EXTERNALSYM ttbThumbBottomHot}
  ttbThumbBottomPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbBottomPressed; {$EXTERNALSYM ttbThumbBottomPressed}
  ttbThumbBottomFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbBottomFocused; {$EXTERNALSYM ttbThumbBottomFocused}
  ttbThumbBottomDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbBottomDisabled; {$EXTERNALSYM ttbThumbBottomDisabled}
  ttbThumbTopNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTopNormal; {$EXTERNALSYM ttbThumbTopNormal}
  ttbThumbTopHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTopHot; {$EXTERNALSYM ttbThumbTopHot}
  ttbThumbTopPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTopPressed; {$EXTERNALSYM ttbThumbTopPressed}
  ttbThumbTopFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTopFocused; {$EXTERNALSYM ttbThumbTopFocused}
  ttbThumbTopDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTopDisabled; {$EXTERNALSYM ttbThumbTopDisabled}
  ttbThumbVertNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbVertNormal; {$EXTERNALSYM ttbThumbVertNormal}
  ttbThumbVertHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbVertHot; {$EXTERNALSYM ttbThumbVertHot}
  ttbThumbVertPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbVertPressed; {$EXTERNALSYM ttbThumbVertPressed}
  ttbThumbVertFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbVertFocused; {$EXTERNALSYM ttbThumbVertFocused}
  ttbThumbVertDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbVertDisabled; {$EXTERNALSYM ttbThumbVertDisabled}
  ttbThumbLeftNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbLeftNormal; {$EXTERNALSYM ttbThumbLeftNormal}
  ttbThumbLeftHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbLeftHot; {$EXTERNALSYM ttbThumbLeftHot}
  ttbThumbLeftPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbLeftPressed; {$EXTERNALSYM ttbThumbLeftPressed}
  ttbThumbLeftFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbLeftFocused; {$EXTERNALSYM ttbThumbLeftFocused}
  ttbThumbLeftDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbLeftDisabled; {$EXTERNALSYM ttbThumbLeftDisabled}
  ttbThumbRightNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbRightNormal; {$EXTERNALSYM ttbThumbRightNormal}
  ttbThumbRightHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbRightHot; {$EXTERNALSYM ttbThumbRightHot}
  ttbThumbRightPressed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbRightPressed; {$EXTERNALSYM ttbThumbRightPressed}
  ttbThumbRightFocused = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbRightFocused; {$EXTERNALSYM ttbThumbRightFocused}
  ttbThumbRightDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbRightDisabled; {$EXTERNALSYM ttbThumbRightDisabled}
  ttbThumbTics = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTics; {$EXTERNALSYM ttbThumbTics}
  ttbThumbTicsVert = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttbThumbTicsVert; {$EXTERNALSYM ttbThumbTicsVert}

// TThemedTrayNotify
const
  ttnTrayNotifyDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttnTrayNotifyDontCare; {$EXTERNALSYM ttnTrayNotifyDontCare}
  ttnTrayNotifyRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttnTrayNotifyRoot; {$EXTERNALSYM ttnTrayNotifyRoot}
  ttnBackground = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttnBackground; {$EXTERNALSYM ttnBackground}
  ttnAnimBackground = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttnAnimBackground; {$EXTERNALSYM ttnAnimBackground}

// TThemedTreeview
const
  ttTreeviewDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTreeviewDontCare; {$EXTERNALSYM ttTreeviewDontCare}
  ttTreeviewRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttTreeviewRoot; {$EXTERNALSYM ttTreeviewRoot}
  ttItemNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttItemNormal; {$EXTERNALSYM ttItemNormal}
  ttItemHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttItemHot; {$EXTERNALSYM ttItemHot}
  ttItemSelected = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttItemSelected; {$EXTERNALSYM ttItemSelected}
  ttItemDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttItemDisabled; {$EXTERNALSYM ttItemDisabled}
  ttItemSelectedNotFocus = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttItemSelectedNotFocus; {$EXTERNALSYM ttItemSelectedNotFocus}
  ttGlyphClosed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttGlyphClosed; {$EXTERNALSYM ttGlyphClosed}
  ttGlyphOpened = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttGlyphOpened; {$EXTERNALSYM ttGlyphOpened}
  ttBranch = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.ttBranch; {$EXTERNALSYM ttBranch}

// TThemedWindow
const
  twWindowDontCare = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twWindowDontCare; {$EXTERNALSYM twWindowDontCare}
  twWindowRoot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twWindowRoot; {$EXTERNALSYM twWindowRoot}
  twCaptionActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCaptionActive; {$EXTERNALSYM twCaptionActive}
  twCaptionInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCaptionInactive; {$EXTERNALSYM twCaptionInactive}
  twCaptionDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCaptionDisabled; {$EXTERNALSYM twCaptionDisabled}
  twSmallCaptionActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCaptionActive; {$EXTERNALSYM twSmallCaptionActive}
  twSmallCaptionInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCaptionInactive; {$EXTERNALSYM twSmallCaptionInactive}
  twSmallCaptionDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCaptionDisabled; {$EXTERNALSYM twSmallCaptionDisabled}
  twMinCaptionActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinCaptionActive; {$EXTERNALSYM twMinCaptionActive}
  twMinCaptionInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinCaptionInactive; {$EXTERNALSYM twMinCaptionInactive}
  twMinCaptionDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinCaptionDisabled; {$EXTERNALSYM twMinCaptionDisabled}
  twSmallMinCaptionActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallMinCaptionActive; {$EXTERNALSYM twSmallMinCaptionActive}
  twSmallMinCaptionInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallMinCaptionInactive; {$EXTERNALSYM twSmallMinCaptionInactive}
  twSmallMinCaptionDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallMinCaptionDisabled; {$EXTERNALSYM twSmallMinCaptionDisabled}
  twMaxCaptionActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxCaptionActive; {$EXTERNALSYM twMaxCaptionActive}
  twMaxCaptionInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxCaptionInactive; {$EXTERNALSYM twMaxCaptionInactive}
  twMaxCaptionDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxCaptionDisabled; {$EXTERNALSYM twMaxCaptionDisabled}
  twSmallMaxCaptionActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallMaxCaptionActive; {$EXTERNALSYM twSmallMaxCaptionActive}
  twSmallMaxCaptionInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallMaxCaptionInactive; {$EXTERNALSYM twSmallMaxCaptionInactive}
  twSmallMaxCaptionDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallMaxCaptionDisabled; {$EXTERNALSYM twSmallMaxCaptionDisabled}
  twFrameLeftActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameLeftActive; {$EXTERNALSYM twFrameLeftActive}
  twFrameLeftInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameLeftInactive; {$EXTERNALSYM twFrameLeftInactive}
  twFrameRightActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameRightActive; {$EXTERNALSYM twFrameRightActive}
  twFrameRightInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameRightInactive; {$EXTERNALSYM twFrameRightInactive}
  twFrameBottomActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameBottomActive; {$EXTERNALSYM twFrameBottomActive}
  twFrameBottomInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameBottomInactive; {$EXTERNALSYM twFrameBottomInactive}
  twSmallFrameLeftActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameLeftActive; {$EXTERNALSYM twSmallFrameLeftActive}
  twSmallFrameLeftInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameLeftInactive; {$EXTERNALSYM twSmallFrameLeftInactive}
  twSmallFrameRightActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameRightActive; {$EXTERNALSYM twSmallFrameRightActive}
  twSmallFrameRightInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameRightInactive; {$EXTERNALSYM twSmallFrameRightInactive}
  twSmallFrameBottomActive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameBottomActive; {$EXTERNALSYM twSmallFrameBottomActive}
  twSmallFrameBottomInactive = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameBottomInactive; {$EXTERNALSYM twSmallFrameBottomInactive}
  twSysButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSysButtonNormal; {$EXTERNALSYM twSysButtonNormal}
  twSysButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSysButtonHot; {$EXTERNALSYM twSysButtonHot}
  twSysButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSysButtonPushed; {$EXTERNALSYM twSysButtonPushed}
  twSysButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSysButtonDisabled; {$EXTERNALSYM twSysButtonDisabled}
  twMDISysButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDISysButtonNormal; {$EXTERNALSYM twMDISysButtonNormal}
  twMDISysButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDISysButtonHot; {$EXTERNALSYM twMDISysButtonHot}
  twMDISysButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDISysButtonPushed; {$EXTERNALSYM twMDISysButtonPushed}
  twMDISysButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDISysButtonDisabled; {$EXTERNALSYM twMDISysButtonDisabled}
  twMinButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinButtonNormal; {$EXTERNALSYM twMinButtonNormal}
  twMinButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinButtonHot; {$EXTERNALSYM twMinButtonHot}
  twMinButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinButtonPushed; {$EXTERNALSYM twMinButtonPushed}
  twMinButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMinButtonDisabled; {$EXTERNALSYM twMinButtonDisabled}
  twMDIMinButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIMinButtonNormal; {$EXTERNALSYM twMDIMinButtonNormal}
  twMDIMinButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIMinButtonHot; {$EXTERNALSYM twMDIMinButtonHot}
  twMDIMinButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIMinButtonPushed; {$EXTERNALSYM twMDIMinButtonPushed}
  twMDIMinButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIMinButtonDisabled; {$EXTERNALSYM twMDIMinButtonDisabled}
  twMaxButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxButtonNormal; {$EXTERNALSYM twMaxButtonNormal}
  twMaxButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxButtonHot; {$EXTERNALSYM twMaxButtonHot}
  twMaxButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxButtonPushed; {$EXTERNALSYM twMaxButtonPushed}
  twMaxButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMaxButtonDisabled; {$EXTERNALSYM twMaxButtonDisabled}
  twCloseButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCloseButtonNormal; {$EXTERNALSYM twCloseButtonNormal}
  twCloseButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCloseButtonHot; {$EXTERNALSYM twCloseButtonHot}
  twCloseButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCloseButtonPushed; {$EXTERNALSYM twCloseButtonPushed}
  twCloseButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCloseButtonDisabled; {$EXTERNALSYM twCloseButtonDisabled}
  twSmallCloseButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCloseButtonNormal; {$EXTERNALSYM twSmallCloseButtonNormal}
  twSmallCloseButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCloseButtonHot; {$EXTERNALSYM twSmallCloseButtonHot}
  twSmallCloseButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCloseButtonPushed; {$EXTERNALSYM twSmallCloseButtonPushed}
  twSmallCloseButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCloseButtonDisabled; {$EXTERNALSYM twSmallCloseButtonDisabled}
  twMDICloseButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDICloseButtonNormal; {$EXTERNALSYM twMDICloseButtonNormal}
  twMDICloseButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDICloseButtonHot; {$EXTERNALSYM twMDICloseButtonHot}
  twMDICloseButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDICloseButtonPushed; {$EXTERNALSYM twMDICloseButtonPushed}
  twMDICloseButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDICloseButtonDisabled; {$EXTERNALSYM twMDICloseButtonDisabled}
  twRestoreButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twRestoreButtonNormal; {$EXTERNALSYM twRestoreButtonNormal}
  twRestoreButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twRestoreButtonHot; {$EXTERNALSYM twRestoreButtonHot}
  twRestoreButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twRestoreButtonPushed; {$EXTERNALSYM twRestoreButtonPushed}
  twRestoreButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twRestoreButtonDisabled; {$EXTERNALSYM twRestoreButtonDisabled}
  twMDIRestoreButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIRestoreButtonNormal; {$EXTERNALSYM twMDIRestoreButtonNormal}
  twMDIRestoreButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIRestoreButtonHot; {$EXTERNALSYM twMDIRestoreButtonHot}
  twMDIRestoreButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIRestoreButtonPushed; {$EXTERNALSYM twMDIRestoreButtonPushed}
  twMDIRestoreButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIRestoreButtonDisabled; {$EXTERNALSYM twMDIRestoreButtonDisabled}
  twHelpButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHelpButtonNormal; {$EXTERNALSYM twHelpButtonNormal}
  twHelpButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHelpButtonHot; {$EXTERNALSYM twHelpButtonHot}
  twHelpButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHelpButtonPushed; {$EXTERNALSYM twHelpButtonPushed}
  twHelpButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHelpButtonDisabled; {$EXTERNALSYM twHelpButtonDisabled}
  twMDIHelpButtonNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIHelpButtonNormal; {$EXTERNALSYM twMDIHelpButtonNormal}
  twMDIHelpButtonHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIHelpButtonHot; {$EXTERNALSYM twMDIHelpButtonHot}
  twMDIHelpButtonPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIHelpButtonPushed; {$EXTERNALSYM twMDIHelpButtonPushed}
  twMDIHelpButtonDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twMDIHelpButtonDisabled; {$EXTERNALSYM twMDIHelpButtonDisabled}
  twHorzScrollNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzScrollNormal; {$EXTERNALSYM twHorzScrollNormal}
  twHorzScrollHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzScrollHot; {$EXTERNALSYM twHorzScrollHot}
  twHorzScrollPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzScrollPushed; {$EXTERNALSYM twHorzScrollPushed}
  twHorzScrollDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzScrollDisabled; {$EXTERNALSYM twHorzScrollDisabled}
  twHorzThumbNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzThumbNormal; {$EXTERNALSYM twHorzThumbNormal}
  twHorzThumbHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzThumbHot; {$EXTERNALSYM twHorzThumbHot}
  twHorzThumbPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzThumbPushed; {$EXTERNALSYM twHorzThumbPushed}
  twHorzThumbDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twHorzThumbDisabled; {$EXTERNALSYM twHorzThumbDisabled}
  twVertScrollNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertScrollNormal; {$EXTERNALSYM twVertScrollNormal}
  twVertScrollHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertScrollHot; {$EXTERNALSYM twVertScrollHot}
  twVertScrollPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertScrollPushed; {$EXTERNALSYM twVertScrollPushed}
  twVertScrollDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertScrollDisabled; {$EXTERNALSYM twVertScrollDisabled}
  twVertThumbNormal = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertThumbNormal; {$EXTERNALSYM twVertThumbNormal}
  twVertThumbHot = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertThumbHot; {$EXTERNALSYM twVertThumbHot}
  twVertThumbPushed = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertThumbPushed; {$EXTERNALSYM twVertThumbPushed}
  twVertThumbDisabled = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twVertThumbDisabled; {$EXTERNALSYM twVertThumbDisabled}
  twDialog = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twDialog; {$EXTERNALSYM twDialog}
  twCaptionSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twCaptionSizingTemplate; {$EXTERNALSYM twCaptionSizingTemplate}
  twSmallCaptionSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallCaptionSizingTemplate; {$EXTERNALSYM twSmallCaptionSizingTemplate}
  twFrameLeftSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameLeftSizingTemplate; {$EXTERNALSYM twFrameLeftSizingTemplate}
  twSmallFrameLeftSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameLeftSizingTemplate; {$EXTERNALSYM twSmallFrameLeftSizingTemplate}
  twFrameRightSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameRightSizingTemplate; {$EXTERNALSYM twFrameRightSizingTemplate}
  twSmallFrameRightSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameRightSizingTemplate; {$EXTERNALSYM twSmallFrameRightSizingTemplate}
  twFrameBottomSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twFrameBottomSizingTemplate; {$EXTERNALSYM twFrameBottomSizingTemplate}
  twSmallFrameBottomSizingTemplate = {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.twSmallFrameBottomSizingTemplate; {$EXTERNALSYM twSmallFrameBottomSizingTemplate}


{$IFNDEF COMPILER16_UP}
type
  TElementSize = (esMinimum, esActual, esStretch);
{$ELSE}
  esMinimum = TElementSize.esStretch;
  esActual = TElementSize.esActual;
  esStretch = TElementSize.esStretch;
{$ENDIF ~COMPILER16_UP}

type
  TThemeServicesEx = class(TThemeServices)
  {$IFNDEF COMPILER16_UP}
  private
    function DoGetElementSize(DC: HDC; Details: TThemedElementDetails; Rect: PRect;
      ElementSize: TElementSize; out Size: TSize): Boolean;
  {$ENDIF ~COMPILER16_UP}
  public
    {$IFNDEF COMPILER7_UP}
    procedure ApplyThemeChange;
    {$ENDIF ~COMPILER7_UP}
    {$IFNDEF COMPILER16_UP}
    function GetElementContentRect(DC: HDC; Details: TThemedElementDetails;
      const BoundingRect: TRect; out AContentRect: TRect): Boolean;
    function GetElementSize(DC: HDC; Details: TThemedElementDetails; ElementSize: TElementSize;
      out Size: TSize): Boolean; overload;
    function GetElementSize(DC: HDC; Details: TThemedElementDetails; const Rect: TRect;
      ElementSize: TElementSize; out Size: TSize): Boolean; overload;
    function IsSystemStyle: Boolean;
    function Enabled: Boolean;
    function Available: Boolean;
    function GetSystemColor(Color: TColor): TColor;
    {$ENDIF ~COMPILER16_UP}
  end;

function ThemeServices: TThemeServicesEx;
function StyleServices: TThemeServicesEx;

{ PaintControlBorder paints the themed border for WinControls only when they
  have the WS_EX_CLIENTEDGE. }
procedure PaintControlBorder(Control: TWinControl);

{ DrawThemedBorder draws a teEditTextNormal element (border) to the DC. It uses
  the Control's BoundsRect. DrawThemedBorder forces border painting. }
procedure DrawThemedBorder(Control: TControl);

{$ENDIF JVCLThemesEnabled}

type
  {$IFDEF COMPILER7_UP}
  TJvThemeStyle = TControlStyle;
  {$ELSE}
  TJvThemeStyle = set of (csNeedsBorderPaint, csParentBackground);
  {$ENDIF COMPILER7_UP}

{
  Instead of the ControlStyle property you should use the following functions:

    ControlStyle := ControlStyle + [csXxx]; -> IncludeThemeStyle(Self, [csXxx]);
    ControlStyle := ControlStyle - [csXxx]; -> ExcludeThemeStyle(Self, [csXxx]);
    if csXxx in ControlStyle then           -> if csXxx in GetThemeStyle(Self) then

}
procedure IncludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
procedure ExcludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
function GetThemeStyle(Control: TControl): TJvThemeStyle;

{ DrawThemedBackground fills R with Canvas.Brush.Color/Color. If the control uses
  csParentBackground and the color is that of it's parent the Rect is not filled
  because then it is done by the JvThemes/VCL7. }
procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; NeedsParentBackground: Boolean = True); overload;
procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; Color: TColor; NeedsParentBackground: Boolean = True); overload;
procedure DrawThemedBackground(Control: TControl; DC: HDC; const R: TRect;
  Brush: HBRUSH; NeedsParentBackground: Boolean = True); overload;

{ DrawThemesFrameControl draws a themed frame control when theming is enabled. }
function DrawThemedFrameControl(DC: HDC; const Rect: TRect; uType, uState: UINT): BOOL;


{ PerformEraseBackground sends a WM_ERASEBKGND message to the Control's parent. }
procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint; const R: TRect); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC); overload;
procedure PerformEraseBackground(Control: TControl; DC: HDC; const R: TRect); overload;


{ DrawThemedButtonFace draws a themed button when theming is enabled. }
function DrawThemedButtonFace(Control: TControl; Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; Style: TButtonStyle; IsRounded, IsDown,
  IsFocused, IsHot: Boolean): TRect;

{ IsMouseOver returns True if the mouse is over the control. }
function IsMouseOver(Control: TControl): Boolean;

// ~COMPILER7_UP: These functions are helpers for Delphi 6 that doesn't have the csParentPackground flag.
{ GetParentBackground returns True if the Control has the csParentPackground
  ControlStyle }
function GetParentBackground(Control: TWinControl): Boolean;
{ SetParentBackground sets the Control's csParentPackground ControlStyle }
procedure SetParentBackground(Control: TWinControl; Value: Boolean);

{ GetGlassPaintFlag returns True if csGlassPaint in ControlState }
function GetGlassPaintFlag(AControl: TControl): Boolean;
{ ControlInGlassPaint returns True if the Control is painted on a glass area }
function ControlInGlassPaint(AControl: TControl): Boolean;
{ DrawGlassableText paints text to a device context with support of PaintOnGlass }
procedure DrawGlassableText(DC: HDC; const Text: string; var TextRect: TRect; TextFlags: Cardinal;
  PaintOnGlass: Boolean = False);
{ DrawGlassableImageList paint a transparent imagelist image to the canvas with
  support of PaintOnGlass }
procedure DrawGlassableImageList(ImageList: HIMAGELIST; Index: Integer; Dest: HDC; X, Y: Integer;
  Style: UINT; PaintOnGlass: Boolean = False);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
{$IFNDEF COMPILER10_UP}
  JclSysUtils,
{$ENDIF ~COMPILER10_UP}
  JclSysInfo;

type
  TWinControlThemeInfo = class(TWinControl)
  public
    property Color;
  end;

procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; NeedsParentBackground: Boolean = True);
begin
  DrawThemedBackground(Control, Canvas, R, Canvas.Brush.Color, NeedsParentBackground);
end;

procedure DrawThemedBackground(Control: TControl; Canvas: TCanvas;
  const R: TRect; Color: TColor; NeedsParentBackground: Boolean = True);
var
  Cl: TColor;
begin
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled and
     (Control.Parent <> nil) and
     ((Color = TWinControlThemeInfo(Control.Parent).Color) or
      (ColorToRGB(Color) = ColorToRGB(TWinControlThemeInfo(Control.Parent).Color))) and
     (not NeedsParentBackground or (csParentBackground in GetThemeStyle(Control))) then
  begin
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, Canvas.Handle, R)
      else
        StyleServices.DrawParentBackground(TWinControl(Control).Handle, Canvas.Handle, nil,
          False, @R);
    end
    else
      PerformEraseBackground(Control, Canvas.Handle, R)
  end
  else
  {$ENDIF JVCLThemesEnabled}
  begin
    {$IFDEF JVCLStylesEnabled}
    if StyleServices.Enabled and TStyleManager.IsCustomStyleActive then
      Color := StyleServices.GetSystemColor(Color);
    {$ENDIF JVCLStylesEnabled}
    Cl := Canvas.Brush.Color;
    if Cl <> Color then
      Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
    if Cl <> Canvas.Brush.Color then
      Canvas.Brush.Color := Cl;
  end;
end;

procedure DrawThemedBackground(Control: TControl; DC: HDC; const R: TRect;
  Brush: HBRUSH; NeedsParentBackground: Boolean = True);
{$IFDEF JVCLThemesEnabled}
var
  LogBrush: TLogBrush;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  GetObject(Brush, SizeOf(LogBrush), @LogBrush);
  if StyleServices.Enabled and
     (Control.Parent <> nil) and
     (LogBrush.lbColor = Cardinal(ColorToRGB(TWinControlThemeInfo(Control.Parent).Color))) and
     (not NeedsParentBackground or (csParentBackground in GetThemeStyle(Control))) then
  begin
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, DC, R)
      else
        StyleServices.DrawParentBackground(TWinControl(Control).Handle, DC, nil, False, @R);
    end
    else
      PerformEraseBackground(Control, DC, R)
  end
  else
  {$ENDIF JVCLThemesEnabled}
    FillRect(DC, R, Brush);
end;

function DrawThemedFrameControl(DC: HDC; const Rect: TRect; uType, uState: UINT): BOOL;
{$IFDEF JVCLThemesEnabled}
const
  Mask = $00FF;
var
  Btn: TThemedButton;
  ComboBox: TThemedComboBox;
  ScrollBar: TThemedScrollBar;
  R: TRect;
  Details: TThemedElementDetails;
{$ENDIF JVCLThemesEnabled}
begin
  Result := False;
  {$IFDEF JVCLThemesEnabled}
  if StyleServices.Enabled then
  begin
    R := Rect;
    case uType of
      DFC_BUTTON:
        case uState and Mask of
          DFCS_BUTTONPUSH:
            begin
              if uState and (DFCS_TRANSPARENT or DFCS_FLAT) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbPushButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbPushButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbPushButtonHot
                else
                if uState and DFCS_MONO <> 0 then
                  Btn := tbPushButtonDefaulted
                else
                  Btn := tbPushButtonNormal;

                Details := StyleServices.GetElementDetails(Btn);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            end;
          DFCS_BUTTONCHECK:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbCheckBoxCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbCheckBoxCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbCheckBoxCheckedHot
                else
                  Btn := tbCheckBoxCheckedNormal;
              end
              else
              if uState and DFCS_MONO <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbCheckBoxMixedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbCheckBoxMixedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbCheckBoxMixedHot
                else
                  Btn := tbCheckBoxMixedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbCheckBoxUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbCheckBoxUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbCheckBoxUncheckedHot
                else
                  Btn := tbCheckBoxUncheckedNormal;
              end;
              Details := StyleServices.GetElementDetails(Btn);
              StyleServices.DrawElement(DC, Details, R);
              Result := True;
            end;
          DFCS_BUTTONRADIO:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbRadioButtonCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbRadioButtonCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbRadioButtonCheckedHot
                else
                  Btn := tbRadioButtonCheckedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  Btn := tbRadioButtonUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  Btn := tbRadioButtonUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  Btn := tbRadioButtonUncheckedHot
                else
                  Btn := tbRadioButtonUncheckedNormal;
              end;
              Details := StyleServices.GetElementDetails(Btn);
              StyleServices.DrawElement(DC, Details, R);
              Result := True;
            end;
        end;
      DFC_SCROLL:
        begin
          case uState and Mask of
            DFCS_SCROLLCOMBOBOX:
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ComboBox := tcDropDownButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ComboBox := tcDropDownButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ComboBox := tcDropDownButtonHot
                else
                  ComboBox := tcDropDownButtonNormal;

                Details := StyleServices.GetElementDetails(ComboBox);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLUP:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnUpDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnUpPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnUpHot
                else
                  ScrollBar := tsArrowBtnUpNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLDOWN:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnDownDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnDownPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnDownHot
                else
                  ScrollBar := tsArrowBtnDownNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLLEFT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnLeftDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnLeftPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnLeftHot
                else
                  ScrollBar := tsArrowBtnLeftNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
            DFCS_SCROLLRIGHT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  ScrollBar := tsArrowBtnRightDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  ScrollBar := tsArrowBtnRightPressed
                else
                if uState and DFCS_HOT <> 0 then
                  ScrollBar := tsArrowBtnRightHot
                else
                  ScrollBar := tsArrowBtnRightNormal;

                Details := StyleServices.GetElementDetails(ScrollBar);
                StyleServices.DrawElement(DC, Details, R);
                Result := True;
              end;
          end;
        end;
    end;
  end;
  {$ENDIF JVCLThemesEnabled}

  if not Result then
    Result := DrawFrameControl(DC, Rect, uType, uState);
end;

function IsInvalidRect(const R: TRect): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := (R.Left = MaxInt) and (R.Top = MaxInt) and (R.Right = MaxInt) and (R.Bottom = MaxInt);
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint);
begin
  PerformEraseBackground(Control, DC, Offset, Rect(MaxInt, MaxInt, MaxInt, MaxInt));
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; Offset: TPoint; const R: TRect);
var
  WindowOrg: TPoint;
  OrgRgn, Rgn: THandle;
  Clipped: Boolean;
  {$IFDEF COMPILER16_UP}
  OldPen: HPEN;
  OldBrush: HBRUSH;
  OldFont: HFONT;
  OldTextColor: TColorRef;
  OldBkMode: Integer;
  {$ENDIF COMPILER16_UP}
begin
  if Control.Parent <> nil then
  begin
    if (Offset.X <> 0) and (Offset.Y <> 0) then
    begin
      GetWindowOrgEx(DC, WindowOrg);
      if Control is TGraphicControl then
        SetWindowOrgEx(DC, -Offset.X, -Offset.Y, nil)
      else
        SetWindowOrgEx(DC, WindowOrg.X + Offset.X, WindowOrg.Y + Offset.Y, nil);
    end;

    Clipped := False;
    OrgRgn := 0;
    if not IsInvalidRect(R) then
    begin
      OrgRgn := CreateRectRgn(0, 0, 1, 1);
      Clipped := GetClipRgn(DC, OrgRgn) = 1;
      Rgn := CreateRectRgnIndirect(R);
      SelectClipRgn(DC, Rgn);
      DeleteObject(Rgn);
    end;

    try
      {$IFDEF COMPILER16_UP}
      // Delphi XE2's Style-Engine has a bug in the TStyleHook.WMEraseBkgnd that replaces the
      // selected GDI objects with the TCanvas default objects ("System" font, ...).
      // We need to repair the damage in order to have the same behavior of the native theming API.
      // General rule for WM_ERASEBKGND: Return the DC in the state in that it was when the function
      // was called.
      OldPen := 0;
      OldBrush := 0;
      OldFont := 0;
      OldTextColor := 0;
      OldBkMode := 0;
      if StyleServices.Enabled and not StyleServices.IsSystemStyle then
      begin
        OldPen := GetCurrentObject(DC, OBJ_PEN);
        OldBrush := GetCurrentObject(DC, OBJ_BRUSH);
        OldFont := GetCurrentObject(DC, OBJ_FONT);
        OldTextColor := GetTextColor(DC);
        OldBkMode := GetBkMode(DC);
      end;
      {$ENDIF COMPILER16_UP}
      Control.Parent.Perform(WM_ERASEBKGND, DC, DC); // force redraw
      {$IFDEF COMPILER16_UP}
      if StyleServices.Enabled and not StyleServices.IsSystemStyle then
      begin
        if GetCurrentObject(DC, OBJ_PEN) <> OldPen then
          SelectObject(DC, OldPen);
        if GetCurrentObject(DC, OBJ_BRUSH) <> OldBrush then
          SelectObject(DC, OldBrush);
        if GetCurrentObject(DC, OBJ_FONT) <> OldFont then
          SelectObject(DC, OldFont);
        if GetTextColor(DC) <> OldTextColor then
          SetTextColor(DC, OldTextColor);
        if GetBkMode(DC) <> OldBkMode then
          SetBkMode(DC, OldBkMode);
      end;
      {$ENDIF COMPILER16_UP}
    finally
      if (Offset.X <> 0) and (Offset.Y <> 0) then
        SetWindowOrgEx(DC, WindowOrg.X, WindowOrg.Y, nil);

      if OrgRgn <> 0 then
      begin
        if Clipped then
          SelectClipRgn(DC, OrgRgn)
        else
          SelectClipRgn(DC, 0);
        DeleteObject(OrgRgn);
      end;
    end;
  end;
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC);
begin
  PerformEraseBackground(Control, DC, Point(Control.Left, Control.Top));
end;

procedure PerformEraseBackground(Control: TControl; DC: HDC; const R: TRect);
begin
  PerformEraseBackground(Control, DC, Point(Control.Left, Control.Top), R);
end;

function DrawThemedButtonFace(Control: TControl; Canvas: TCanvas;
  const Client: TRect; BevelWidth: Integer; Style: TButtonStyle;
  IsRounded, IsDown, IsFocused, IsHot: Boolean): TRect;
{$IFDEF JVCLThemesEnabled}
var
  Btn: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF JVCLThemesEnabled}
begin
  {$IFDEF JVCLThemesEnabled}
  if (Style <> bsWin31) and StyleServices.Enabled then
  begin
    Result := Client;

    if IsDown then
      Btn := tbPushButtonPressed
    else
    if IsFocused then
      Btn := tbPushButtonDefaulted
    else
    if IsHot then
      Btn := tbPushButtonHot
    else
      Btn := tbPushButtonNormal;

    Details := StyleServices.GetElementDetails(Btn);
    StyleServices.DrawElement(Canvas.Handle, Details, Result);
    StyleServices.GetElementContentRect(Canvas.Handle, Details, Client, Result);

    if IsFocused then
      DrawFocusRect(Canvas.Handle, Result);

    InflateRect(Result, -BevelWidth, -BevelWidth);
  end
  else
  {$ENDIF JVCLThemesEnabled}
    Result := DrawButtonFace(Canvas, Client, BevelWidth, Style, IsRounded, IsDown, IsFocused);
end;

function IsMouseOver(Control: TControl): Boolean;
var
  Pt: TPoint;
begin
  Pt := Control.ScreenToClient(Mouse.CursorPos);
  Result := PtInRect(Control.ClientRect, Pt);
end;

function GetParentBackground(Control: TWinControl): Boolean;
begin
  Result := csParentBackground in GetThemeStyle(Control);
end;

procedure SetParentBackground(Control: TWinControl; Value: Boolean);
begin
  if Value <> GetParentBackground(Control) then
  begin
    if Value then
      IncludeThemeStyle(Control, [csParentBackground])
    else
      ExcludeThemeStyle(Control, [csParentBackground]);
    Control.Invalidate;
  end;
end;

function GetGlassPaintFlag(AControl: TControl): Boolean;
{$IFDEF COMPILER11}
var
  Form: TCustomForm;
{$ENDIF COMPILER11}
begin
  {$IFDEF COMPILER12_UP}
  Result := csGlassPaint in AControl.ControlState;
  {$ELSE}
  Result := False;
  {$IFDEF COMPILER11}
  Form := GetParentForm(AControl);
  if (Form <> nil) and Form.GlassFrame.Enabled then
    Result := Form.GlassFrame.IntersectsControl(AControl);
  {$ENDIF COMPILER11}
  {$ENDIF COMPILER12_UP}
end;

function ControlInGlassPaint(AControl: TControl): Boolean;
{$IFDEF COMPILER11_UP}
var
  Parent: TWinControl;
{$ENDIF COMPILER11_UP}
begin
  {$IFDEF COMPILER11_UP}
  Result := GetGlassPaintFlag(AControl);
  if Result then
  begin
    Parent := AControl.Parent;
    while (Parent <> nil) and not Parent.DoubleBuffered and not (Parent is TCustomForm) do
      Parent := Parent.Parent;
    Result := (Parent = nil) or not Parent.DoubleBuffered or (Parent is TCustomForm);
  end;
  {$ELSE}
  Result := False;
  {$ENDIF COMPILER11_UP}
end;

procedure DrawGlassableText(DC: HDC; const Text: string; var TextRect: TRect; TextFlags: Cardinal;
  PaintOnGlass: Boolean = False);
{$IFDEF COMPILER11_UP}
var
  Options: TDTTOpts;
  {$IFDEF COMPILER11}
  S: WideString;
  {$ENDIF COMPILER11}
{$ENDIF COMPILER11_UP}
begin
  {$IFDEF COMPILER11_UP}
  if StyleServices.Enabled and JclCheckWinVersion(6, 0) then
  begin
    FillChar(Options, SizeOf(Options), 0);
    Options.dwSize := SizeOf(Options);
    if TextFlags and DT_CALCRECT <> 0 then
      Options.dwFlags := Options.dwFlags or DTT_CALCRECT;
    if PaintOnGlass then
      Options.dwFlags := Options.dwFlags or DTT_COMPOSITED;
    Options.dwFlags := Options.dwFlags or DTT_TEXTCOLOR;
    Options.crText := GetTextColor(DC);

    {$IFDEF COMPILER16_UP}
    if not StyleServices.IsSystemStyle then
    begin
      // The Style engine doesn't have DrawThemeTextEx support
      {$WARNINGS OFF} // ignore "deprecated" warning
      StyleServices.DrawText(DC, StyleServices.GetElementDetails(tbPushButtonNormal), Text, TextRect, TextFlags, 0);
      {$WARNINGS ON}
      Exit;
    end
    else
    {$ENDIF}
    begin
      {$IFDEF COMPILER12_UP}
      with ThemeServices do
        if DrawThemeTextEx(Theme[teToolBar], DC, TP_BUTTON, TS_NORMAL, PWideChar(Text), Length(Text),
                           TextFlags, TextRect, Options) <> E_NOTIMPL then
          Exit;
      {$ELSE}
      S := Text;
      with ThemeServices do
        if DrawThemeTextEx(Theme[teToolBar], DC, TP_BUTTON, TS_NORMAL, PWideChar(S), Length(S),
                           TextFlags, @TextRect, Options) <> E_NOTIMPL then
          Exit;
      {$ENDIF COMPILER12_UP}
    end;
  end;
  {$ENDIF COMPILER11_UP}
  Windows.DrawText(DC, PChar(Text), Length(Text), TextRect, TextFlags);
end;

procedure DrawGlassableImageList(ImageList: HIMAGELIST; Index: Integer; Dest: HDC; X, Y: Integer;
  Style: UINT; PaintOnGlass: Boolean = False);
{$IFDEF COMPILER11_UP}
var
  PaintBuffer: HPAINTBUFFER;
  R: TRect;
  MemDC, MaskDC: HDC;
  CX, CY, XX, YY: Integer;
  MaskBmp: TBitmap;
{$ENDIF COMPILER11_UP}
begin
  {$IFDEF COMPILER11_UP}
  if PaintOnGlass and JclCheckWinVersion(6, 0) then
  begin
    { TODO : Not working correctly on a JvSpeedButton. But it works if used direcly on
             a sheet of glass. Some optimizations could be done. }

    ImageList_GetIconSize(ImageList, CX, CY);
    R := Rect(X, Y, X + CX, Y + CY);

    PaintBuffer := BeginBufferedPaint(Dest, R, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      ImageList_Draw(ImageList, Index, MemDC, X, Y, Style);
      BufferedPaintMakeOpaque(PaintBuffer, @R);

      MaskBmp := TBitmap.Create;
      try
        MaskBmp.Width := CX;
        MaskBmp.Height := CY;
        MaskDC := MaskBmp.Canvas.Handle;
        ImageList_Draw(ImageList, Index, MaskDC, 0, 0, ILD_MASK);
        for YY := 0 to CY - 1 do
          for XX := 0 to CX - 1 do
            if GetPixel(MaskDC, XX, YY) <> 0 then
            begin
              R := Rect(X + XX, Y + YY, X + XX + 1, Y + YY + 1);
              BufferedPaintSetAlpha(PaintBuffer, @R, 0);
              //SetPixel(MemDC, X + XX, Y + YY, GetPixel(MemDC, X + XX, Y + YY) and $00FFFFFF);
            end;
      finally
        MaskBmp.Free;
      end;
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end
  else
  {$ENDIF COMPILER11_UP}
    ImageList_Draw(ImageList, Index, Dest, X, Y, Style);
end;

{$IFDEF JVCLThemesEnabled}

{$IFNDEF COMPILER7_UP}
procedure TThemeServicesEx.ApplyThemeChange;
begin
  StyleServices.UpdateThemes;
  StyleServices.DoOnThemeChange;
end;
{$ENDIF ~COMPILER7_UP}

{$IFNDEF COMPILER16_UP}
function TThemeServicesEx.GetElementContentRect(DC: HDC; Details: TThemedElementDetails;
  const BoundingRect: TRect; out AContentRect: TRect): Boolean;
begin
  AContentRect := ContentRect(DC, Details, BoundingRect);
  Result := True;
end;

function TThemeServicesEx.DoGetElementSize(DC: HDC; Details: TThemedElementDetails; Rect: PRect;
  ElementSize: TElementSize; out Size: TSize): Boolean;
const
  ElementSizes: array[TElementSize] of TThemeSize = (TS_MIN, TS_TRUE, TS_DRAW);
begin
  Result := GetThemePartSize(Theme[Details.Element], DC, Details.Part, Details.State, Rect,
    ElementSizes[ElementSize], Size) = S_OK;
end;

function TThemeServicesEx.GetElementSize(DC: HDC; Details: TThemedElementDetails; ElementSize: TElementSize;
  out Size: TSize): Boolean;
begin
  Result := DoGetElementSize(DC, Details, nil, ElementSize, Size);
end;

function TThemeServicesEx.GetElementSize(DC: HDC; Details: TThemedElementDetails; const Rect: TRect;
  ElementSize: TElementSize; out Size: TSize): Boolean;
begin
  Result := DoGetElementSize(DC, Details, @Rect, ElementSize, Size);
end;

function TThemeServicesEx.GetSystemColor(Color: TColor): TColor;
begin
  Result := Color;
end;

function TThemeServicesEx.IsSystemStyle: Boolean;
begin
  Result := True;
end;

function TThemeServicesEx.Enabled: Boolean;
begin
  Result := ThemesEnabled;
end;

function TThemeServicesEx.Available: Boolean;
begin
  Result := ThemesAvailable;
end;
{$ENDIF ~COMPILER16_UP}

function ThemeServices: TThemeServicesEx;
begin
  Result := TThemeServicesEx(
    {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.{$IFDEF RTL230_UP}StyleServices{$ELSE}ThemeServices{$ENDIF RTL230_UP});
end;

function StyleServices: TThemeServicesEx;
begin
  Result := TThemeServicesEx(
    {$IFDEF COMPILER7_UP}Themes{$ELSE}ThemeSrv{$ENDIF}.{$IFDEF RTL230_UP}StyleServices{$ELSE}ThemeServices{$ENDIF RTL230_UP});
end;

procedure PaintControlBorder(Control: TWinControl);
begin
  StyleServices.PaintBorder(Control, False)
end;

procedure DrawThemedBorder(Control: TControl);
var
  Details: TThemedElementDetails;
  DrawRect: TRect;
  DC: HDC;
  Handle: THandle;
begin
  if Control is TWinControl then
  begin
    Handle := TWinControl(Control).Handle;
    DC := GetWindowDC(Handle);
    GetWindowRect(Handle, DrawRect);
    OffsetRect(DrawRect, -DrawRect.Left, -DrawRect.Top);
  end
  else
  begin
    if Control.Parent = nil then
      Exit;
    Handle := Control.Parent.Handle;
    DC := GetDC(Handle);
    DrawRect := Control.BoundsRect;
  end;

  ExcludeClipRect(DC, DrawRect.Left + 2, DrawRect.Top + 2, DrawRect.Right - 2, DrawRect.Bottom - 2);
  Details := StyleServices.GetElementDetails(teEditTextNormal);
  StyleServices.DrawElement(DC, Details, DrawRect);

  ReleaseDC(Handle, DC);
end;

{$IFDEF COMPILER7_UP}

{ Delphi 7 and newer handle these styles itself. }

type
  TControlAccessProtected = class(TControl);

procedure IncludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
  with TControlAccessProtected(Control) do
    ControlStyle := ControlStyle + (Style * [csNeedsBorderPaint, csParentBackground]);
end;

procedure ExcludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
  with TControlAccessProtected(Control) do
    ControlStyle := ControlStyle - (Style * [csNeedsBorderPaint, csParentBackground]);
end;

function GetThemeStyle(Control: TControl): TJvThemeStyle;
begin
  with TControlAccessProtected(Control) do
    Result := ControlStyle * [csNeedsBorderPaint, csParentBackground];
end;

{$ELSE} // COMPILER7_UP

{ Delphi 5 and 6 need WindowProc hooks }

type
  THookStatus = (hsNone, hsInWndProc, hsDelete);

  TThemeHook = class(TObject)
  public
    FControl: TControl;
    FStatus: THookStatus;
    FWndProcCount: Integer;
    FDead: Boolean;
    FThemeStyle: TJvThemeStyle;
    FOrgWndProc: TWndMethod;

    procedure WndProc(var Msg: TMessage);
  protected
    procedure ThemedPaint(var Msg: TWMPaint; var Handled: Boolean);
    procedure ThemedNCPaint(var Msg: TWMNCPaint);
    procedure ThemedEraseBkgnd(var Msg: TWMEraseBkgnd; var Handled: Boolean);
    procedure ThemedCtlColorStatic(var Msg: TWMCtlColorStatic; var Handled: Boolean);
  public
    constructor Create(AControl: TControl);
    destructor Destroy; override;
    procedure DeleteHook;

    procedure IncludeThemeStyle(Style: TJvThemeStyle);
    procedure ExcludeThemeStyle(Style: TJvThemeStyle);

    property Control: TControl read FControl;
    property ThemeStyle: TJvThemeStyle read FThemeStyle;
  end;

  { TThemeHookList contains all ThemeHooks. }
  TThemeHookList = class(TObjectList)
  private
    FLock: TRTLCriticalSection;
    FRecreationList: TList;
    FDeadList: TObjectList;
    FEraseBkgndHooked: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enter;
    procedure Leave;

    function FindControl(Control: TControl): TThemeHook;

    { GetControl returns the TThemeHook for the Control. If there is no item
      it creates a new one. }
    function GetControl(Control: TControl): TThemeHook;

    property RecreationList: TList read FRecreationList;
  end;

  { TThemeHookComponent is responsible for unhooking TGraphicControls. }
  TThemeHookComponent = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

// global ThemeHook list
var
  GlobalThemeHooks: TThemeHookList = nil;
  ThemeHookComponent: TThemeHookComponent = nil;
  WinControlHookInstalled: Boolean = False;

procedure InstallWinControlHook; forward;
procedure UninstallWinControlHook; forward;

function ThemeHooks: TThemeHookList;
begin
  if not Assigned(GlobalThemeHooks) then
    GlobalThemeHooks := TThemeHookList.Create;
  Result := GlobalThemeHooks;
end;

//=== { TThemeHookList } =====================================================

constructor TThemeHookList.Create;
begin
  inherited Create;
  FRecreationList := TList.Create;
  FDeadList := TObjectList.Create;
  InitializeCriticalSection(FLock);

  ThemeHookComponent := TThemeHookComponent.Create(nil); // global variable
end;

destructor TThemeHookList.Destroy;
begin
  FRecreationList.Free;
  FDeadList.Free;
  DeleteCriticalSection(FLock);

  Clear; // destroy TThemeHook instances which require ThemeHookComponent
  ThemeHookComponent.Free; // global variable

  inherited Destroy;
end;

procedure TThemeHookList.Enter;
begin
  EnterCriticalSection(FLock);
end;

procedure TThemeHookList.Leave;
begin
  LeaveCriticalSection(FLock);
end;

function TThemeHookList.FindControl(Control: TControl): TThemeHook;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TThemeHook(Items[I]);
    if Result.FControl = Control then
      Exit;
  end;
  Result := nil;
end;

function TThemeHookList.GetControl(Control: TControl): TThemeHook;
var
  I: Integer;
begin
  Result := FindControl(Control);
  if Result = nil then
  begin
    for I := 0 to FDeadList.Count - 1 do
    begin
      Result := TThemeHook(FDeadList[I]);
      if Result.Control = Control then
      begin
        Result.FDead := False;
        if (Result.FControl <> nil) and not (csDesigning in Result.FControl.ComponentState) then
          if Result.FControl is TGraphicControl then
            Result.FControl.FreeNotification(ThemeHookComponent);
        FDeadList.Extract(Result);
        Add(Result);
        Exit;
      end;
    end;
    Result := TThemeHook.Create(Control);
    Add(Result);
  end;
end;

//=== { TThemeHook } =========================================================

constructor TThemeHook.Create(AControl: TControl);
begin
  inherited Create;
  FControl := AControl;
  if not (csDesigning in FControl.ComponentState) then
  begin
    FOrgWndProc := FControl.WindowProc;
    FControl.FreeNotification(ThemeHookComponent);
    FControl.WindowProc := WndProc;
    if not WinControlHookInstalled then
      InstallWinControlHook;
  end;
end;

destructor TThemeHook.Destroy;
begin
  if FControl <> nil then
    FControl.RemoveFreeNotification(ThemeHookComponent);
  inherited Destroy;
end;

procedure TThemeHook.DeleteHook;
begin
  if (FControl <> nil) and not (csDesigning in FControl.ComponentState) then
  begin
    if TMethod(FControl.WindowProc).Code = @TThemeHook.WndProc then
      FControl.WindowProc := FOrgWndProc
    else
    if not (TMethod(FControl.WindowProc).Code = TMethod(FOrgWndProc).Code) then
      FDead := True; // keep WndProc
    FControl.RemoveFreeNotification(ThemeHookComponent);
    FControl := nil;
  end;
  if FStatus = hsInWndProc then
    FStatus := hsDelete;
  if FStatus = hsDelete then
    Exit;
  if not FDead then
    ThemeHooks.RecreationList.Remove(FControl);
  ThemeHooks.Enter;
  try
    if FDead then
    begin
      ThemeHooks.Extract(Self);
      ThemeHooks.FDeadList.Add(Self);
    end
    else
      ThemeHooks.Remove(Self)
  finally
    ThemeHooks.Leave;
  end;
end;

procedure TThemeHook.IncludeThemeStyle(Style: TJvThemeStyle);
begin
  FThemeStyle := FThemeStyle + Style;
end;

procedure TThemeHook.ExcludeThemeStyle(Style: TJvThemeStyle);
begin
  FThemeStyle := FThemeStyle - Style;
  if FThemeStyle = [] then
    DeleteHook;
end;

procedure TThemeHook.WndProc(var Msg: TMessage);
var
  Handled: Boolean;
begin
  // Should not happen but it can if the WindowProc is hooked by another component
  if ThemeHooks = nil then
    Exit;
  if FDead then
  begin
    FOrgWndProc(Msg);
    Exit;
  end;

  Handled := False;
  case Msg.Msg of
    CM_RECREATEWND:
      if ThemeHooks.RecreationList.IndexOf(Control) = -1 then
        ThemeHooks.RecreationList.Add(Control);
    WM_PAINT:
      if StyleServices.Enabled then
        ThemedPaint(TWMPaint(Msg), Handled);
    WM_ERASEBKGND:
      if StyleServices.Enabled then
        ThemedEraseBkgnd(TWMEraseBkgnd(Msg), Handled);
    CN_CTLCOLORSTATIC, CN_CTLCOLORBTN:
      if StyleServices.Enabled then
        ThemedCtlColorStatic(TWMCtlColorStatic(Msg), Handled);
  end;

  Inc(FWndProcCount);
  try
    FStatus := hsInWndProc;
    if not Handled then
      FOrgWndProc(Msg);
  finally
    Dec(FWndProcCount);
    if (FStatus = hsDelete) and (FWndProcCount <= 0) then
    begin
      FStatus := hsNone;
      DeleteHook;
      if Msg.Msg = WM_DESTROY then
        Msg.Msg := 0;
    end
  end;

  case Msg.Msg of
    WM_NCPAINT:
      if StyleServices.Enabled then
        ThemedNCPaint(TWMNCPaint(Msg));
    WM_DESTROY:
      if (csDestroying in Control.ComponentState) and (ThemeHooks.RecreationList.IndexOf(Control) = -1) then
        DeleteHook;
  end;

  while ThemeHooks.RecreationList.Count > 0 do
  begin
    TWinControl(ThemeHooks.RecreationList[0]).HandleNeeded;
    ThemeHooks.RecreationList.Delete(0);
  end;
end;

procedure TThemeHook.ThemedPaint(var Msg: TWMPaint; var Handled: Boolean);
begin
  if Control is TGraphicControl then
    if csParentBackground in ThemeStyle then
      PerformEraseBackground(Control, Msg.DC);
end;

procedure TThemeHook.ThemedNCPaint(var Msg: TWMNCPaint);
begin
  if csNeedsBorderPaint in ThemeStyle then
    if Control is TWinControl then
    begin
      StyleServices.PaintBorder(TWinControl(Control), False);
      Msg.Result := 0;
    end;
end;

procedure TThemeHook.ThemedEraseBkgnd(var Msg: TWMEraseBkgnd; var Handled: Boolean);
begin
  if ThemeHooks.FEraseBkgndHooked then
    Exit;

  if csParentBackground in ThemeStyle then
    if Control is TWinControl then
    begin
      if TWinControl(Control).DoubleBuffered then
        PerformEraseBackground(Control, Msg.DC)
      else
        StyleServices.DrawParentBackground(TWinControl(Control).Handle, Msg.DC, nil, False);
      Msg.Result := 1;
      Handled := True;
    end;
end;

procedure TThemeHook.ThemedCtlColorStatic(var Msg: TWMCtlColorStatic; var Handled: Boolean);
begin
  if csParentBackground in ThemeStyle then
  begin
    if Control is TWinControl then
    begin
      ThemedEraseBkgnd(TWMEraseBkgnd(Msg), Handled);
      Msg.Result := GetStockObject(NULL_BRUSH);
    end;
  end;
end;

//=== { TThemeHookComponent } ================================================

procedure TThemeHookComponent.Notification(AComponent: TComponent; Operation: TOperation);
var
  ThemeHook: TThemeHook;
begin
  if Operation = opRemove then
  begin
    ThemeHooks.Enter;
    try
      ThemeHook := ThemeHooks.FindControl(TControl(AComponent));
    finally
      ThemeHooks.Leave;
    end;
    if ThemeHook <> nil then
      ThemeHook.DeleteHook;
  end;
end;

//=== functions ==============================================================

procedure IncludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
  if Style <> [] then
  begin
    ThemeHooks.Enter;
    try
      ThemeHooks.GetControl(Control).IncludeThemeStyle(Style);
    finally
      ThemeHooks.Leave;
    end;
  end;
end;

procedure ExcludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
  if Style <> [] then
  begin
    ThemeHooks.Enter;
    try
      ThemeHooks.GetControl(Control).ExcludeThemeStyle(Style);
    finally
      ThemeHooks.Leave;
    end;
  end;
end;

function GetThemeStyle(Control: TControl): TJvThemeStyle;
var
  ThemeHook: TThemeHook;
begin
  ThemeHooks.Enter;
  try
    ThemeHook := ThemeHooks.FindControl(Control);
    if Assigned(ThemeHook) then
      Result := ThemeHook.ThemeStyle
    else
      Result := [];
  finally
    ThemeHooks.Leave;
  end;
end;

procedure WMEraseBkgndHook(Self: TWinControl; var Msg: TWMEraseBkgnd);
var
  R: TRect;
begin
  if not Self.DoubleBuffered or (Msg.DC = HDC(Msg.Unused)) then
  begin
    if StyleServices.Enabled and (csParentBackground in GetThemeStyle(Self)) then
    begin
      R := Self.ClientRect;
      StyleServices.DrawParentBackground(Self.Handle, Msg.DC, nil, False, @R);
    end
    else
      FillRect(Msg.DC, Self.ClientRect, Self.Brush.Handle);
  end;
  Msg.Result := 1;
end;

type
  TJumpCode = packed record
    Pop: Byte; // pop xxx
    Jmp: Byte; // jmp Offset
    Offset: Integer;
  end;
  PJumpCode = ^TJumpCode;

var
  SavedWinControlCode: TJumpCode;

procedure InstallWinControlHook;
var
  Code: TJumpCode;
  P: procedure;
  N: Cardinal;
begin
  if WinControlHookInstalled then
    Exit;

  P := GetDynamicMethod(TWinControl, WM_ERASEBKGND);
  if Assigned(P) then
  begin
    if PByte(@P)^ = $53 then // push ebx
      Code.Pop := $5B // pop ebx                           
    else
    if PByte(@P)^ = $55 then // push ebp
      Code.Pop := $5D // pop ebp
    else
      Exit;

    Code.Jmp := $E9;
    Code.Offset := PAnsiChar(@WMEraseBkgndHook) - (PAnsiChar(@P) + 1) - SizeOf(Code);

    { The strange thing is that the $e9 cannot be overriden with a "PUSH xxx" }
    if ReadProcessMemory(GetCurrentProcess, Pointer(PAnsiChar(@P) + 1),
                         @SavedWinControlCode, SizeOf(SavedWinControlCode), N) and
      WriteProtectedMemory(Pointer(PAnsiChar(@P) + 1), @Code, SizeOf(Code), N) then
    begin
      WinControlHookInstalled := True;
      ThemeHooks.FEraseBkgndHooked := True;
    end;
  end;
end;

procedure UninstallWinControlHook;
var
  P: procedure;
  OldProtect, Dummy: Cardinal;
begin
  if not WinControlHookInstalled then
    Exit;

  P := GetDynamicMethod(TWinControl, WM_ERASEBKGND);
  if Assigned(P) then
  begin
    // Pointer(Cardinal(@P): Delphi 5 and 6 are 32bit only, so no problem here with 64bit code
    if VirtualProtect(Pointer(Cardinal(@P) + 1), SizeOf(SavedWinControlCode), PAGE_EXECUTE_READWRITE,
                      OldProtect) then
    try
      PJumpCode(Cardinal(@P) + 1)^ := SavedWinControlCode;
      WinControlHookInstalled := False;
      FlushInstructionCache(GetCurrentProcess, @P, SizeOf(SavedWinControlCode));
    finally
      VirtualProtect(Pointer(Cardinal(@P) + 1), SizeOf(SavedWinControlCode), OldProtect, Dummy);
    end;
  end;
end;

{$ENDIF COMPILER7_UP}

{$ELSE} // JVCLThemesEnabled

procedure IncludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
end;

procedure ExcludeThemeStyle(Control: TControl; Style: TJvThemeStyle);
begin
end;

function GetThemeStyle(Control: TControl): TJvThemeStyle;
begin
  Result := [];
end;

{$ENDIF JVCLThemesEnabled}

{$IFDEF JVCLThemesEnabled}

{$IFNDEF COMPILER10_UP}
type
  PPointer = ^Pointer;

var
  OrgWinControlWMPrintClient: procedure(Instance: TObject; var Msg: TMessage);

procedure FixedWMPrintClient(Instance: TObject; var Msg: TMessage);
var
  IdSave: Integer;
begin
  if Msg.Msg = WM_PRINTCLIENT then
  begin
    IdSave := SaveDC(HDC(Msg.WParam));
    try
      OrgWinControlWMPrintClient(Instance, Msg);
    finally
      RestoreDC(HDC(Msg.WParam), IdSave);
    end;
  end
  else
    OrgWinControlWMPrintClient(Instance, Msg);
end;

function FindWMPrintClient: PPointer;
var
  IdxList: PDynamicIndexList;
  I: Integer;
begin
  IdxList := GetDynamicIndexList(TWinControl);
  for I := 0 to GetDynamicMethodCount(TWinControl) - 1 do
    if IdxList[I] = WM_PRINTCLIENT then
    begin
      Result := @(GetDynamicAddressList(TWinControl)[I]);
      Exit;
    end;
  Result := nil;
end;

procedure InitializeWMPrintClientFix;
var
  NewProc: Pointer;
  Proc: PPointer;
  OldProtect, Dummy: Cardinal;
begin
  Proc := FindWMPrintClient();
  if Proc <> nil then
  begin
    OrgWinControlWMPrintClient := Proc^;
    NewProc := @FixedWMPrintClient;

    if VirtualProtect(Proc, SizeOf(NewProc), PAGE_EXECUTE_READWRITE, OldProtect) then
    try
      Proc^ := NewProc;
    finally
      VirtualProtect(Proc, SizeOf(NewProc), OldProtect, Dummy);
    end;
  end;
end;

procedure FinalizeWMPrintClientFix;
var
  NewProc: Pointer;
  Proc: PPointer;
  OldProtect, Dummy: Cardinal;
begin
  Proc := FindWMPrintClient;
  if Proc <> nil then
  begin
    NewProc := @OrgWinControlWMPrintClient;

    if VirtualProtect(Proc, SizeOf(NewProc), PAGE_EXECUTE_READWRITE, OldProtect) then
    try
      Proc^ := NewProc;
    finally
      VirtualProtect(Proc, SizeOf(NewProc), OldProtect, Dummy);
    end;
  end;
end;
{$ENDIF ~COMPILER10_UP}

{$ENDIF JVCLThemesEnabled}

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

  {$IFDEF JVCLThemesEnabled}
  {$IFNDEF COMPILER10_UP}
  InitializeWMPrintClientFix;
  {$ENDIF ~COMPILER10_UP}
  {$ENDIF JVCLThemesEnabled}

finalization
  {$IFDEF JVCLThemesEnabled}

  {$IFNDEF COMPILER10_UP}
  FinalizeWMPrintClientFix;
  {$ENDIF ~COMPILER10_UP}

  {$IFNDEF COMPILER7_UP}
  FreeAndNil(GlobalThemeHooks);
  UninstallWinControlHook;
  {$ENDIF !COMPILER7_UP}
  {$ENDIF JVCLThemesEnabled}

  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
