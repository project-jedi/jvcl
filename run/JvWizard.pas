{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvWizard.PAS, released on 2001-12-30.

The Initial Developer of the Original Code is William Yu Wei.
Portions created by William Yu Wei are Copyright (C) 2001 William Yu Wei.
All Rights Reserved.

Contributor(s):
Peter Thörnqvist - converted to JVCL naming conventions on 2003-07-11
Andreas Hausladen - fixed some bugs, refactoring of the Wizard button classes on 2004-02-29

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{+---------------------------------------------------------------------------+
 | CONTRIBUTORS:                                                             |
 +---------------------------------------------------------------------------+
 |   Steve Forbes          <ozmosys att mira dott net>                                |
 |   Chris Macksey         <c_macksey att hotmail dott com>                           |
 |   Wayne Niddery         <waynen att logicfundamentals dott com>                    |
 |   Raymond J. Schappe    <rschappe att isthmus-ts dott com>                         |
 |   Theodore              <thpana att otenet dott gr>                                |
 |   Max Evans             <max att codecraft dott com dott au>                            |
 +---------------------------------------------------------------------------+
 | HISTORY      COMMENTS                                                     |
 +---------------------------------------------------------------------------+
 | 02/18/2002   OnStartButtonClick, OnLastButtonClick, OnNextButtonClick,    |
 |              OnBackButtonClick, OnFinishButtonClick, OnCancelButtonClick, |
 |              is added for TJvWizardCustomPage with a Stop parameter.      |
 |                                                                           |
 |              Note: these page click events is called before the wizard    |
 |                    button click events.                                   |
 |                                                                           |
 | 02/16/2002   Suggested by <Theodore>:                                     |
 |                1) ModalResult property is added for TJvWizardButton       |
 |                2) Width property is added for TJvWizardButton             |
 |                                                                           |
 | 02/12/2002   1) Suggested by <Max Evans>:                                 |
 |                  Having the next/finish buttons to be the default         |
 |                  button when a page shows.                                |
 |              2) Having the cancel button to be the default cancel button. |
 |                                                                           |
 | 02/11/2002   1) CanDisplay function is added for TJvWizardRouteMapControl.|
 |              2) OnDisplaying event is added for TJvWizardRouteMapControl, |
 |                 so the route map can decide if it could display the page  |
 |                item or not.                                               |
 |                                                                           |
 | 02/10/2002   1) Introduce TJvWizardImage class.                           |
 |              2) Clean up the code (TJvWizardGraphicObject,                |
 |                   TJvWizardPageObject, ..., etc)                          |
 |              3) Now the TJvWizardPageTitle is inherited from              |
 |                 TJvWizardGraphicObject(known as TJvWizardPersistent       |
 |                 in former).                                               |
 |              4) Suggested by <Tim Schneider>:                             |
 |                  Controls in the wizard page with aligned set to          |
 |                  something should be aligned properly without covering    |
 |                  the page header as well as the watermark. Hint:          |
 |                    overrided AdjustClientRect method is added for         |
 |                    both TJvWizardCustomPage and TJvWizardWelcomePage.     |
 |                                                                           |
 | 02/09/2002   1) Finish button can be displayed separatly.                 |
 |              2) Bug fixed: Changing the value of EnabledButtons property  |
 |                 of TJvWizardCustomPage at run time doesn't refresh the    |
 |                 buttons' status on the screen.                            |
 |                                                                           |
 | 02/08/2002   Bug fixed: the OnEnterPage event is not triggled properly,   |
 |                they would be called at the time the wizard is loading     |
 |                them. 'not (csLoading in ComponentState)' added as the     |
 |                part of checking condition in SetActivePage method         |
 |                of TJvWizard.                                              |
 |                                                                           |
 | 02/07/2002   VERSION 1.6 RELEASED                                         |
 |                                                                           |
 |              1) New property EnabledButtons and VisibleButtons added for  |
 |                 TJvWizardCustomPage, so the developers can customize      |
 |                 buttons for each page at design time and run time.        |
 |              2) Remove Enabled and Visible properties from                |
 |                 TJvWizardButton of TJvWizard.                             |
 |                                                                           |
 | 02/06/2002   1) Bug fixed: change TJvWizardWelcomePage's color from       |
 |                 clWindow to other colors or change                        |
 |                 TJvWizardInteriorPage's color from other colors to        |
 |                 clWindow, the pages won't display in correct color.       |
 |                 Hint:                                                     |
 |                   By assigning default value of Color property in         |
 |                   published section of TJvWizardCustomPage and            |
 |                   TJvWizardWelcomePage class.                             |
 |              2) Suggested by <Steve Forbes>:                              |
 |                   ShowDivider added for TJvWizardPageHeader to enable     |
 |                   or disable drawing the page header divider.             |
 |              3) Use Object.Free instead of FreeAndNil,                    |
 |                 Rectangle(ARect.Left, ...) instead of Rectangle(ARect),   |
 |                 so we can support Delphi 4.                               |
 |                                                                           |
 | 02/05/2002   1) Added by <Theodore>:                                      |
 |                  ButtonHelp added for TJvWizard.                          |
 |              2) RepositionButtons method of TJvWizard is improved.        |
 |                                                                           |
 | 02/04/2002   function IsForward added, return true if FromPage is         |
 |              forward to ToPage, return false if FromPage is backward      |
 |              to ToPage.                                                   |
 |                                                                           |
 | 02/03/2002   1) Bug fixed by <Theodore>: SelectPriorPage calls            |
 |                   OnSelectFirstPage event rather the OnSelectPriorPage.   |
 |              2) Suggested by <Theodore>:                                  |
 |                   FromPage parameters added for OnEnterPage event,        |
 |                   so the developers can detect from where it enters.      |
 |              3) Suggested by <Theodore>:                                  |
 |                   ToPage paramters added for OnExitPage event, so the     |
 |                   developers can detect to where it exits.                |
 |              4) Suggested by <Theodore>:                                  |
 |                   OnExitPage event now is called just BEFORE (not after)  |
 |                   the page is hidden and BEFORE the new page is actived.  |
 |                   It provides the last chance to the developers to stop   |
 |                   changing to the new page by raising a message.          |
 |                                                                           |
 | 02/02/2002   VERSION 1.5 RELEASED                                         |
 |                                                                           |
 |              1) DoAddPage, DoDeletePage, DoUpdatePage, DoMovePage added   |
 |                 for TJvWizardRouteMapControl                              |
 |              2) Overrided SetParent added for TJvWizardRouteMapControl    |
 |                   to detect if the parent is TJvWizard or its descentants.|
 | 01/31/2002   1) Improved the RepositionButtons method of TJvWizard,       |
 |                 so all the buttons can be positioned properly regardless  |
 |                 how their neighbors are.                                  |
 |              2) CM_VisibleChanged message handler added for               |
 |                 TJvWizardButtonControl, so when the button is visible or  |
 |                 invisible, it can make the rest buttons in proper         |
 |                 position.                                                 |
 |                                                                           |
 | 01/30/2002   1) Rename the methods of TJvWizardRouteMapControl            |
 |              2) WizardPageMoved method added for TJvWizardRouteMapControl |
 |                 which fired after the order of the page changed.          |
 |              3) OnPaintPage event added for TJvWizardCustomPage, so       |
 |                 the developers can custom draw the page.                  |
 |              4) A TJvWizardCustomPage parameter added for IsFirstPage,    |
 |                 IsLastPage of TJvWizard to test if the specific page is   |
 |                 the first page or the last page.                          |
 |              5) Buttons property added for TJvWizardCustomPage, it can    |
 |                 easily access all navigation buttons of TJvWizard.        |
 |              6) Improved the process to handle the button visible         |
 |                 property in more efficent way.                            |
 |                   see UpdateButtonsStatus method of TJvWizard             |
 |                                                                           |
 | 01/29/2002   1) Pages property added for TJvWizard.                       |
 |              2) PageCount property added for TJvWizard.                   |
 |              3) Page List Property Editor added for Pages property        |
 |                of TJvWizard. From this property editor, we can            |
 |                                                                           |
 |                  a) Add new wizard pages.                                 |
 |                  b) Remove selected pages.                                |
 |                  c) Drag drop selected page item to change pages' order.  |
 |                                                                           |
 | 01/28/2002   1) Bug fixed: if the current active page set to disabled,    |
 |                            the wizard would not go to next page.          |
 |              2) Page screen flicker problem solved by setting             |
 |                 the DoubleBuffered property of TJvWizardCustomPage        |
 |                 to True.                                                  |
 |              3) ParentFont property added for TJvWizardPageHeader.        |
 |                                                                           |
 | 01/27/2002   VERSION 1.5 BETA RELEASED                                    |
 |                                                                           |
 |              1) JvWizard About form added by <Steve Forbes>               |
 |                   Thanks for his great job !!!!                           |
 |              2) Improve the design time button function, press Back       |
 |                 button at first page will forward to the last page.       |
 |                 While press Next button at last page will forward to      |
 |                 the first page. (See FindNextPage method in TJvWizard)    |
 |              3) Fixed AV when delete only one page in the wizard at       |
 |                 design time. (see RemovePage method in TJvWizard)         |
 |              4) NumGlyphs property added for TJvWizardNavigateButton by   |
 |                 <Steve Forbes>, to solve the problem where the            |
 |                 NumGlyphs property of the actual button always reset      |
 |                 to 1 when it is created dynamically.                      |
 |              5) Layout property added for TJvWizardNavigateButton.        |
 |              6) Set ImageAlign property's default value of                |
 |                 TJvWizardPageHeader to waRight.                           |
 |                                                                           |
 | 01/26/2002   1) Suggested by <Steve Forbes>:                              |
 |                   Anchors, AnchorPlacement, Indent property added for     |
 |                   the text in TJvWizardPageTitle. Remove Left, Top,       |
 |                   Width, Height properties from TJvWizardPageTitle. so    |
 |                   it is much easiler to operate the title and subtitle.   |
 |              2) Image property added for TJvWizardCustomPage,             |
 |                 both Welcome page and Interior page can display a         |
 |                 background image.                                         |
 |              3) Image property added for the TJvWizardWaterMark.          |
 |              4) ImageIndex, ImageAlign, ImageOffset property added for    |
 |                 TJvWizardPageHeader. the PageHeader use ImageIndex        |
 |                 to retreive image from the header image list of           |
 |                 TJvWizard.                                                |
 |                                                                           |
 | 01/25/2002   VERSION 1.2 RELEASED                                         |
 |                                                                           |
 |                Finally, JvWizard has its offical icon!!! It is very cool! |
 |                Thanks <Steve Forbes> for his great job !!!!               |
 |                                                                           |
 |              1) Move OnEnterPage, OnPage, OnExitPage event from TJvWizard |
 |                 into TJvWizardCustomPage.                                 |
 |              2) TJvWizardPagePanel added, suggested by <Steve Forbes>.    |
 |              3) Glyph property added for TJvWizardNavigateButton.         |
 |              4) HeaderImages property added for TJvWizard, it is an       |
 |                 image list, which stores all the page header images.      |
 |                                                                           |
 | 01/24/2002   1) Rename TJvWizardTitle to TJvWizardPageTitle.              |
 |              2) PaintTo method added for TJvWizardWaterMark.              |
 |                 PaintTo method added for TJvWizardPageHeader.             |
 |                 PainTo method added for TJvWizardPageTitle.               |
 |              3) Remove the DisplayPageHeader method from                  |
 |                 TJvWizardCustomPage.                                      |
 |              4) OnPage event added for TJvWizard, fired after the page    |
 |                 shows up.                                                 |
 |              5) Pages, PageCount, PageIndex property, and default code    |
 |                 added for all virtual method for TJvWizardRouteMapControl.|
 |              6) Compiler directive added, suggested                       |
 |                 by <Raymond J. Schappe>.                                  |
 |              7) Handle Design time package and Run time package,          |
 |                 package file name convenstion suggested by                |
 |                 <Steve Forbes>:                                           |
 |                   Design time package: JvWizardD?.dpk (bpl, dcp, ...)     |
 |                   Run time package: JvWizardD?R.dbp (bpl, dcp, ...)       |
 |                   here the ? = Delphi Version (5, 6, ..., etc)            |
 |                                                                           |
 | 01/23/2002   1) Start Page, Last Page buttons added for TJvWizard,        |
 |                 default they are invisible.                               |
 |              2) Visible property added for TJvWizardNavigateButton.       |
 |                                                                           |
 | 01/22/2002   BorderWidth property added for TJvWizardWaterMark, suggested |
 |              by <Steve Forbes>                                            |
 |                                                                           |
 |              1) Remove the TJvWizardButtonBar, now all the navigate       |
 |                 buttons are located in the Wizard. Hint:                  |
 |                   Add overrided AdjustClientRect for TJvWizard.           |
 |              2) Bug fixed: Add csAcceptsControls control style into       |
 |                   TJvWizard, otherwise it won't accept other controls     |
 |                   like JvWizardRouteMap.                                  |
 |              3) Bug fixed: TJvWizard.GetChildren procedure, it won't      |
 |                   display another controls (include JvWizardRouteMap      |
 |                   Control) even if the control is in the wizard.          |
 |              4) Align property added for TJvWizardRouteMap, so the        |
 |                 JvWizardRouteMap can display at either left or right      |
 |                 side of the Wizard.                                       |
 |              5) Align property added for TKWaterMark, so it can be        |
 |                 displayed at either left or right side of Welcome Page.   |
 |                                                                           |
 | 01/21/2002   VERSION 1.1 RELEASED                                         |
 |                                                                           |
 |              Suggested by <Chris Macksey>:                                |
 |                                                                           |
 |              1) Add OnSelectNextPage, OnSelectPriorPage,                  |
 |                 OnSelectFirstPage, OnSelectLastPage events, so user can   |
 |                 redirect the page try to go to.                           |
 |              2) Add OnEnterPage, triggled before the page shows up.       |
 |                 Add OnExitPage, triggled after the page is hidded.        |
 |                                                                           |
 | 01/14/2002   1) Add ShowRouteMap property for the TJvWizard.              |
 |              2) Add destructor in the TJvWizardRouteMap class to fix      |
 |                 AV when browse pages after destroy the TJvWizardRouteMap  |
 |                 component.                                                |
 |                                                                           |
 | 01/13/2002   Make the TJvWizardRouteMap as a separat new component        |
 |              so the user can design its own routemap and communicate      |
 |              with TJvWizard smoothly.                                     |
 |                                                                           |
 | 01/12/2002   VERSION 1.0 RELEASED                                         |
 |                                                                           |
 |              1) Fixed by <Wayne Niddery> :                                |
 |                   Under certain circumstance, the Wizard did not always   |
 |                   default to the first page. Add overrided                |
 |                   Loaded method in the TJvWizard class.                   |
 |              2) Restructure: add TJvWizardHeader and TJvWizardWaterMark,  |
 |                 I hate to list all properites like: HeaderColor,          |
 |                 HeaderWidth, HeaderVisible, ... etc. Instead, I group     |
 |                 them together into particular class, and it can make      |
 |                 the whole component has a very clean property             |
 |                 structure.                                                |
 |                                                                           |
 | 01/11/2002   1) Add word break feature when display title and subtitle.   |
 |              2) At Design time, display page name in the wizard page.     |
 |              3) Let the TWizardCustomPage paint and fill its area first   |
 |                 and let TWizardWelcomePage and TWizardInteriorPage        |
 |                 do the rest.                                              |
 |                                                                           |
 | 01/10/2002   BETA VERSION RELEASED                                        |
 |                                                                           |
 |              1) Delete BackButton, NextButton, FinishButton,              |
 |                 CancelButton property, instead of a Button array.         |
 |              2) Introduce TJvWizardBackButton, TJvWizardNextButton,       |
 |                 TJvWizardFinishButton and TJvWizardCancelButton Control   |
 |              3) Add TJvWizardTitle class, HeaderColor, HeaderHeight,      |
 |                 HeaderVisible property for TJvWizardCustomPage.           |
 |              4) Add WaterMarkColor, WaterMarkWidth, WaterMarkVisible      |
 |                 property for TJvWizardWelcomePage.                        |
 |              5) Paint method of TJvWizardWelcomPage improved,             |
 |                 TJvWizardInteriorPage, so they can display header         |
 |                 as well as title and subtitle.                            |
 |                                                                           |
 | 01/06/2002   1) Add TJvWizardRouteMap, Improve all existing functions     |
 |                 and class.                                                |
 |              2) Add TJvWizardCustomPage.                                  |
 |                                                                           |
 | 01/05/2002   1) Add TJvWizardNavigateButton class.                        |
 |              2) Add BackButton, NextButton, FinishButton,                 |
 |                 CancelButton property for TJvWizard.                      |
 |                                                                           |
 | 01/04/2002   1) Add ShowDivider property for TJvWizard.                   |
 |              2) Add GetButtonClick, SetButtonClick for TJvWizardButtonBar.|
 |              3) Draw divider in fsGroove frame style.                     |
 |                                                                           |
 | 12/30/2001   Initial create.                                              |
 +---------------------------------------------------------------------------+
 | TODO LIST                                                                 |
 +---------------------------------------------------------------------------+
 | Wizard page can be transparent                                            |
 +---------------------------------------------------------------------------+}

{$I jvcl.inc}

unit JvWizard;

interface

uses
  SysUtils, Classes,
  Windows, Messages, Controls, Forms, Graphics, Buttons, ImgList,
  {$IFDEF VCL}
  {$IFDEF COMPILER6_UP}
  Types,
  {$ENDIF COMPILER6_UP}
  {$ENDIF VCL}
  {$IFDEF USEJVCL}
  JvComponent,
  {$ENDIF USEJVCL}
  JvWizardCommon;

type
  TJvWizardAboutInfoForm = (JvWizardAbout); // Added by <Steve Forbes>
  TJvWizardAlign = alTop..alRight;
  TJvWizardLeftRight = alLeft..alRight;
  TJvWizardButtonKind =
    (bkStart, bkLast, bkBack, bkNext, bkFinish, bkCancel, bkHelp);
  TJvWizardButtonSet = set of TJvWizardButtonKind;

const
  bkAllButtons = [bkStart, bkLast, bkBack, bkFinish, bkNext, bkCancel, bkHelp];

type
  TJvWizardCustomPage = class;
  TJvWizard = class;

  TJvWizardButtonControl = class(TBitBtn)
  private
    FWizard: TJvWizard;
    FAlignment: TJvWizardLeftRight;
    {$IFDEF VCL}
    procedure CMVisibleChanged(var Msg: TMessage); message CM_VISIBLECHANGED;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF VCL}
  protected
    function StoreCaption: Boolean; virtual;
    {$IFDEF VisualCLX}
    procedure VisibleChanged; override;
    {$ENDIF VisualCLX}
    property Wizard: TJvWizard read FWizard write FWizard;
    property Alignment: TJvWizardLeftRight read FAlignment write FAlignment;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TJvWizardButtonControlClass = class of TJvWizardButtonControl;

  { YW - The wrapper of the TJvWizardButtonControl }
  TJvWizardNavigateButton = class(TPersistent)
  private
    FControl: TJvWizardButtonControl;
    procedure SetCaption(const Value: string);
    function GetCaption: string;
    function GetGlyph: TBitmap;
    procedure SetGlyph(const Value: TBitmap);
    function GetNumGlyphs: Integer;
    procedure SetNumGlyphs(const Value: Integer);
    function GetLayout: TButtonLayout;
    procedure SetLayout(const Value: TButtonLayout);
    function GetModalResult: TModalResult;
    procedure SetModalResult(const Value: TModalResult);
    function GetButtonWidth: Integer;
    procedure SetButtonWidth(const Value: Integer);
  protected
    function StoreCaption: Boolean; virtual;
    property Control: TJvWizardButtonControl read FControl write FControl;
  published
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Caption: string read GetCaption write SetCaption stored StoreCaption;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs;
    property Layout: TButtonLayout read GetLayout write SetLayout default blGlyphLeft;
    property ModalResult: TModalResult read GetModalResult write SetModalResult default mrNone;
    property Width: Integer read GetButtonWidth write SetButtonWidth;
  end;

  TJvWizardRouteMapDisplayEvent = procedure(Sender: TObject;
    const Page: TJvWizardCustomPage; var AllowDisplay: Boolean) of object;

  { YW - TJvWizardRouteMap base class }
  TJvWizardRouteMapControl = class(TCustomControl)
  private
    FWizard: TJvWizard;
    FAlign: TJvWizardAlign;
    FPages: TList;
    FPageIndex: Integer;
    FOnDisplaying: TJvWizardRouteMapDisplayEvent;
    function GetPage(Index: Integer): TJvWizardCustomPage;
    function GetPageCount: Integer;
    procedure SetAlign(Value: TJvWizardAlign);
    procedure SetPageIndex(Value: Integer);
    {$IFDEF VCL}
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF VCL}
    procedure DoAddPage(const APage: TJvWizardCustomPage);
    procedure DoDeletePage(const APage: TJvWizardCustomPage);
    procedure DoUpdatePage(const APage: TJvWizardCustomPage);
    procedure DoActivatePage(const APage: TJvWizardCustomPage);
    procedure DoMovePage(const APage: TJvWizardCustomPage; const OldIndex: Integer);
  protected
    {$IFDEF VCL}
    procedure SetParent(AParent: TWinControl); override;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    procedure SetParent(const ParentA: TWidgetControl); override;
    {$ENDIF VisualCLX}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    function PageAtPos(Pt: TPoint): TJvWizardCustomPage; virtual;
    procedure WizardPageAdded(const APage: TJvWizardCustomPage); virtual;
    procedure WizardPageDeleted(const APage: TJvWizardCustomPage); virtual;
    procedure WizardPageUpdated(const APage: TJvWizardCustomPage); virtual;
    procedure WizardPageActivated(const APage: TJvWizardCustomPage); virtual;
    procedure WizardPageMoved(const APage: TJvWizardCustomPage;
      const OldIndex: Integer); virtual;
    function CanDisplay(const APage: TJvWizardCustomPage): Boolean; virtual;
    property Wizard: TJvWizard read FWizard write FWizard;
    property Align: TJvWizardAlign read FAlign write SetAlign default alLeft;
    property OnDisplaying: TJvWizardRouteMapDisplayEvent
      read FOnDisplaying write FOnDisplaying;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Pages[Index: Integer]: TJvWizardCustomPage read GetPage;
    property PageCount: Integer read GetPageCount;
    property PageIndex: Integer read FPageIndex write SetPageIndex;
  end;

  TJvWizardImage = class(TPersistent)
  private
    FPicture: TPicture;
    FAlignment: TJvWizardImageAlignment;
    FLayout: TJvWizardImageLayout;
    FOnChange: TNotifyEvent;
    FTransparent:boolean;
    procedure SetPicture(Value: TPicture);
    procedure SetAlignment(Value: TJvWizardImageAlignment);
    procedure SetLayout(Value: TJvWizardImageLayout);
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
    procedure DoChange;
    procedure DoPictureChange(Sender:TObject);
  protected
    procedure PaintTo(const ACanvas: TCanvas; ARect: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property Alignment: TJvWizardImageAlignment read FAlignment write SetAlignment default iaStretch;
    property Layout: TJvWizardImageLayout read FLayout write SetLayout default ilStretch;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
  end;

  TJvWizardGraphicObject = class(TPersistent)
  private
    FColor: TColor;
    FVisible: Boolean;
    procedure SetColor(Value: TColor);
    procedure SetVisible(Value: Boolean);
  protected
    procedure VisibleChanged; virtual;
    procedure ColorChanged; virtual;
    procedure DoChange; virtual; abstract;
  public
    constructor Create; virtual;
    procedure PaintTo(ACanvas: TCanvas; var ARect: TRect); virtual; abstract;
  published
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TJvWizardPageHeader = class;

  { YW - Wizard Page Title class }
  TJvWizardPageTitle = class(TJvWizardGraphicObject)
  private
    FWizardPageHeader: TJvWizardPageHeader;
    FText: string;
    FAlignment: TAlignment;
    FAnchorPlacement: Integer;
    FAnchors: TAnchors;
    FIndent: Integer;
    FFont: TFont;
    procedure SetText(const Value: string);
    procedure SetAlignment(Value: TAlignment);
    procedure SetAnchors(Value: TAnchors);
    procedure SetAnchorPlacement(Value: Integer);
    procedure SetIndent(Value: Integer);
    procedure SetFont(Value: TFont);
    procedure SetWizardPageHeader(Value: TJvWizardPageHeader);
    procedure OnChange(Sender: TObject);
    procedure AdjustFont(const AFont: TFont);
  protected
    { YW - Get the area where the title text should be painted on. }
    function GetTextRect(const ACanvas: TCanvas;
      const ARect: TRect): TRect; virtual;
    procedure DoChange; override;
    property WizardPageHeader: TJvWizardPageHeader
      read FWizardPageHeader write SetWizardPageHeader;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PaintTo(ACanvas: TCanvas; var ARect: TRect); override;
  published
    property Text: string read FText write SetText;
    property Anchors: TAnchors read FAnchors write SetAnchors default [akLeft, akTop];
    property AnchorPlacement: Integer read FAnchorPlacement write SetAnchorPlacement default 4;
    property Indent: Integer read FIndent write SetIndent default 0;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
  end;

  TJvWizardPageObject = class(TJvWizardGraphicObject)
  private
    FWizardPage: TJvWizardCustomPage;
    procedure SetWizardPage(Value: TJvWizardCustomPage);
  protected
    procedure Initialize; virtual;
    procedure DoChange; override;
    property WizardPage: TJvWizardCustomPage read FWizardPage write SetWizardPage;
  end;

  { YW - Wizard Page Header class }
  TJvWizardPageHeader = class(TJvWizardPageObject)
  private
    FHeight: Integer;
    FParentFont: Boolean;
    FTitle: TJvWizardPageTitle;
    FSubtitle: TJvWizardPageTitle;
    FImageIndex: Integer;
    FImageOffset: Integer;
    FImageAlignment: TJvWizardImageLeftRight;
    FShowDivider: Boolean;
    procedure SetHeight(Value: Integer);
    procedure SetImageIndex(Value: Integer);
    procedure SetImageOffset(Value: Integer);
    procedure SetImageAlignment(Value: TJvWizardImageLeftRight);
    procedure SetParentFont(Value: Boolean);
    procedure SetShowDivider(Value: Boolean);
    procedure AdjustTitleFont;
  protected
    procedure VisibleChanged; override;
    procedure Initialize; override;
    { YW - the return value of ARect is the area where the title should be
       painted on. The result of GetImageRect is the image area. }
    function GetImageRect(const AImages: TCustomImageList;
      var ARect: TRect): TRect; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PaintTo(ACanvas: TCanvas; var ARect: TRect); override;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property ImageOffset: Integer read FImageOffset write SetImageOffset default 0;
    property ImageAlignment: TJvWizardImageLeftRight read FImageAlignment write SetImageAlignment default iaRight;
    property Height: Integer read FHeight write SetHeight default 70;
    property ParentFont: Boolean read FParentFont write SetParentFont default True;
    property Title: TJvWizardPageTitle read FTitle write FTitle;
    property Subtitle: TJvWizardPageTitle read FSubtitle write FSubtitle;
    property ShowDivider: Boolean read FShowDivider write SetShowDivider default True;
    property Color default clWindow;
    property Visible;
  end;

  { YW - Welcome Page's watermark class }
  TJvWizardWaterMark = class(TJvWizardPageObject)
  private
    FAlign: TJvWizardLeftRight;
    FWidth: Integer;
    FBorderWidth: Integer;
    FImage: TJvWizardImage;
    procedure SetWidth(Value: Integer);
    procedure SetBorderWidth(Value: Integer);
    procedure SetAlign(Value: TJvWizardLeftRight);
    procedure ImageChanged(Sender: TObject);
  protected
    procedure VisibleChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PaintTo(ACanvas: TCanvas; var ARect: TRect); override;
  published
    property Align: TJvWizardLeftRight read FAlign write SetAlign default alLeft;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Image: TJvWizardImage read FImage write FImage;
    property Width: Integer read FWidth write SetWidth default 164;
    {$IFDEF VCL}
    property Color default clActiveCaption;
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    property Color default clActiveHighlight;
    {$ENDIF VisualCLX}
    property Visible;
  end;

  { YW - Wizard Page Panel class used by Wizard Custom Page }
  TJvWizardPagePanel = class(TJvWizardPageObject)
  private
    FBorderWidth: Word;
    procedure SetBorderWidth(Value: Word);
  public
    constructor Create; override;
    procedure PaintTo(ACanvas: TCanvas; var ARect: TRect); override;
  published
    property BorderWidth: Word read FBorderWidth write SetBorderWidth default 7;
    property Color default clBtnFace;
    property Visible;
  end;

  TJvWizardPageClickEvent = procedure(Sender: TObject; var Stop: Boolean) of object;
  TJvWizardPaintPageEvent = procedure(Sender: TObject; ACanvas: TCanvas;
    var ARect: TRect) of object;
  TJvWizardChangePageEvent = procedure(Sender: TObject;
    const FromPage: TJvWizardCustomPage) of object;
  TJvWizardChangingPageEvent = procedure(Sender: TObject;
    var ToPage: TJvWizardCustomPage) of object;

  { YW - Wizard Custom Page }
  TJvWizardCustomPage = class(TCustomControl)
  private
    FWizard: TJvWizard;
    FHeader: TJvWizardPageHeader;
    FPanel: TJvWizardPagePanel;
    FImage: TJvWizardImage;
    FEnabledButtons: TJvWizardButtonSet;
    FVisibleButtons: TJvWizardButtonSet;
    FDrawing: Boolean;
    FEnableJumpToPage: Boolean;    // Nonn
    FOnPaintPage: TJvWizardPaintPageEvent;
    FOnEnterPage: TJvWizardChangePageEvent;
    FOnPage: TNotifyEvent;
    FOnExitPage: TJvWizardChangePageEvent;
    FOnStartButtonClick: TJvWizardPageClickEvent;
    FOnLastButtonClick: TJvWizardPageClickEvent;
    FOnNextButtonClick: TJvWizardPageClickEvent;
    FOnBackButtonClick: TJvWizardPageClickEvent;
    FOnCancelButtonClick: TJvWizardPageClickEvent;
    FOnFinishButtonClick: TJvWizardPageClickEvent;
    FOnHelpButtonClick:TJvWizardPageClickEvent;
    function GetPageIndex: Integer;
    procedure SetPageIndex(const Value: Integer);
    procedure SetWizard(AWizard: TJvWizard);
    procedure SetEnabledButtons(Value: TJvWizardButtonSet);
    procedure SetVisibleButtons(Value: TJvWizardButtonSet);
    procedure ImageChanged(Sender: TObject);
    {$IFDEF VCL}
    // (ahuser) Do not convert to JvExVCL: This package is USEJVCL'ed
    procedure WMEraseBkgnd(var Msg: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    {$ENDIF VCL}
    function GetSubtitle: TJvWizardPageTitle;
    function GetTitle: TJvWizardPageTitle;
    procedure SetSubtitle(const Value: TJvWizardPageTitle);
    procedure SetTitle(const Value: TJvWizardPageTitle);
  protected
    {$IFDEF VisualCLX}
    procedure FontChanged; override;
    procedure TextChanged; override;
    procedure EnabledChanged; override;
    {$ENDIF VisualCLX}
    {$IFDEF VCL}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF VCL}
    procedure ReadState(Reader: TReader); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    { YW - DrawPage is called by paint method. all the derived page controls
       should call this method to paint itsself rather than the overrided
       paint method. }
    procedure DrawPage(ACanvas: TCanvas; var ARect: TRect); virtual;
    { YW - called before the page shows up. Page: From page }
    procedure Enter(const FromPage: TJvWizardCustomPage); virtual;
    { YW - called after the page shows up. }
    procedure Done; virtual;
    { YW - called just before the page is hidden. Page: To page }
    procedure Exit(const ToPage: TJvWizardCustomPage); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EnableButton(AButton: TJvWizardButtonKind; AEnabled: boolean); virtual;
    property Wizard: TJvWizard read FWizard write SetWizard;
    property PageIndex: Integer read GetPageIndex write SetPageIndex stored False;
  published
    property Header: TJvWizardPageHeader read FHeader write FHeader;
    property Subtitle: TJvWizardPageTitle read GetSubtitle write SetSubtitle stored False;
    property Title: TJvWizardPageTitle read GetTitle write SetTitle stored False;
    property Image: TJvWizardImage read FImage write FImage;
    property Panel: TJvWizardPagePanel read FPanel write FPanel;
    property EnabledButtons: TJvWizardButtonSet read FEnabledButtons write SetEnabledButtons default bkAllButtons;
    property VisibleButtons: TJvWizardButtonSet read FVisibleButtons write SetVisibleButtons default [bkBack, bkNext, bkCancel];
    property EnableJumpToPage: Boolean read FEnableJumpToPage write FEnableJumpToPage default True;  // Nonn
    property Color default clBtnFace;
    property Caption;
    property Enabled;
    property Font;
    property Left stored False;
    property Height stored False;
    property PopupMenu;
    property ShowHint;
    property Top stored False;
    property Width stored False;
    property OnEnterPage: TJvWizardChangePageEvent read FOnEnterPage write FOnEnterPage;
    property OnPage: TNotifyEvent read FOnPage write FOnPage;
    property OnExitPage: TJvWizardChangePageEvent read FOnExitPage write FOnExitPage;
    property OnPaintPage: TJvWizardPaintPageEvent read FOnPaintPage write FOnPaintPage;
    property OnStartButtonClick: TJvWizardPageClickEvent read FOnStartButtonClick write FOnStartButtonClick;
    property OnLastButtonClick: TJvWizardPageClickEvent read FOnLastButtonClick write FOnLastButtonClick;
    property OnNextButtonClick: TJvWizardPageClickEvent read FOnNextButtonClick write FOnNextButtonClick;
    property OnBackButtonClick: TJvWizardPageClickEvent read FOnBackButtonClick write FOnBackButtonClick;
    property OnCancelButtonClick: TJvWizardPageClickEvent read FOnCancelButtonClick write FOnCancelButtonClick;
    property OnFinishButtonClick: TJvWizardPageClickEvent read FOnFinishButtonClick write FOnFinishButtonClick;
    property OnHelpButtonClick:TJvWizardPageClickEvent read FOnHelpButtonClick write FOnHelpButtonClick;
  end;

  { YW - Wizard Welcome Page }
  TJvWizardWelcomePage = class(TJvWizardCustomPage)
  private
    FWaterMark: TJvWizardWaterMark;
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DrawPage(ACanvas: TCanvas; var ARect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color default clWindow;
    property WaterMark: TJvWizardWaterMark read FWaterMark write FWaterMark;
  end;

  { YW - Wizard Interior Page }
  TJvWizardInteriorPage = class(TJvWizardCustomPage)
  protected
    procedure DrawPage(ACanvas: TCanvas; var ARect: TRect); override;
  end;

  TJvWizardSelectPageEvent = procedure(Sender: TObject;
    FromPage: TJvWizardCustomPage; var ToPage: TJvWizardCustomPage) of object;

  { YW - JvWizard Page List }
  TJvWizardPageList = class(TList)
  private
    FWizard: TJvWizard;
    function GetItems(Index: Integer): TJvWizardCustomPage;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    property Wizard: TJvWizard read FWizard write FWizard;
  public
    destructor Destroy; override;
    property Items[Index: Integer]: TJvWizardCustomPage read GetItems; default;
  end;

  { YW - JvWizard Control }
  {$IFDEF USEJVCL}
  TJvWizard = class(TJvCustomControl)
  {$ELSE}
  TJvWizard = class(TCustomControl)
  {$ENDIF USEJVCL}
  private
    FPages: TJvWizardPageList;
    FActivePage: TJvWizardCustomPage;
    FRouteMap: TJvWizardRouteMapControl;
    FNavigateButtons: array[TJvWizardButtonKind] of TJvWizardNavigateButton;
    FButtonBarHeight: Integer;
    FShowDivider: Boolean;
    FOnSelectNextPage: TJvWizardSelectPageEvent;
    FOnSelectPriorPage: TJvWizardSelectPageEvent;
    FOnSelectFirstPage: TJvWizardSelectPageEvent;
    FOnSelectLastPage: TJvWizardSelectPageEvent;
    FOnActivePageChanged: TNotifyEvent;
    FOnActivePageChanging: TJvWizardChangingPageEvent;
    FHeaderImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FAutoHideButtonBar: boolean;
    {$IFNDEF USEJVCL}
    FAboutInfo: TJvWizardAboutInfoForm; // Add by Steve Forbes
    {$ENDIF !USEJVCL}
    procedure SetShowDivider(Value: Boolean);
    function GetShowRouteMap: Boolean;
    procedure SetShowRouteMap(Value: Boolean);
    procedure SetButtonBarHeight(Value: Integer);
    procedure SetActivePage(Page: TJvWizardCustomPage);
    procedure SetHeaderImages(Value: TCustomImageList);
    function GetButtonClick(Index: Integer): TNotifyEvent;
    procedure SetButtonClick(Index: Integer; const Value: TNotifyEvent);
    procedure ImageListChange(Sender: TObject);
    procedure CreateNavigateButtons;
    procedure DestroyNavigateButtons;
    procedure ChangeActivePage(Page: TJvWizardCustomPage);
    function GetActivePageIndex: Integer;
    procedure SetActivePageIndex(Value: Integer);
    function GetPageCount: Integer;
    procedure RepositionButtons;
    procedure UpdateButtonsStatus;
    {$IFDEF VCL}
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMDesignHitTest(var Msg: TCMDesignHitTest); message CM_DESIGNHITTEST;
    {$ENDIF VCL}
    function FindNextEnabledPage(PageIndex: Integer; const Step: Integer = 1;
      CheckDisable: Boolean = True): TJvWizardCustomPage;
    procedure SetAutoHideButtonBar(const Value: boolean);
    function GetWizardPages(Index: integer): TJvWizardCustomPage;
  protected
    procedure Loaded; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ShowControl(AControl: TControl); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure InsertPage(Page: TJvWizardCustomPage);
    procedure RemovePage(Page: TJvWizardCustomPage);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetButtonControlClass(AKind: TJvWizardButtonKind): TJvWizardButtonControlClass; virtual;
    procedure DoActivePageChanging(var ToPage: TJvWizardCustomPage); dynamic;
    procedure DoActivePageChanged; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectPriorPage;
    procedure SelectNextPage;
    procedure SelectFirstPage;
    procedure SelectLastPage;
    function IsFirstPage(const APage: TJvWizardCustomPage;
      CheckDisable: Boolean = True): Boolean;
    function IsLastPage(const APage: TJvWizardCustomPage;
      CheckDisable: Boolean = True): Boolean;
    function FindNextPage(PageIndex: Integer; const Step: Integer = 1;
      CheckDisable: Boolean = True): TJvWizardCustomPage;
    function IsForward(const FromPage, ToPage: TJvWizardCustomPage): Boolean;
    property ActivePageIndex: Integer read GetActivePageIndex write SetActivePageIndex;
    property PageCount: Integer read GetPageCount;
    property WizardPages[Index:integer]: TJvWizardCustomPage read GetWizardPages;
  published
    property Pages: TJvWizardPageList read FPages;
    {$IFNDEF USEJVCL}
    // Add by Steve Forbes
    property About: TJvWizardAboutInfoForm read FAboutInfo write FAboutInfo stored False;
    {$ENDIF !USEJVCL}
    property ActivePage: TJvWizardCustomPage read FActivePage write SetActivePage;
    property AutoHideButtonBar:boolean read FAutoHideButtonBar write SetAutoHideButtonBar default True;
    property ButtonBarHeight: Integer read FButtonBarHeight write SetButtonBarHeight;
    property ButtonStart: TJvWizardNavigateButton read FNavigateButtons[bkStart] write FNavigateButtons[bkStart];
    property ButtonLast: TJvWizardNavigateButton read FNavigateButtons[bkLast] write FNavigateButtons[bkLast];
    property ButtonBack: TJvWizardNavigateButton read FNavigateButtons[bkBack] write FNavigateButtons[bkBack];
    property ButtonNext: TJvWizardNavigateButton read FNavigateButtons[bkNext] write FNavigateButtons[bkNext];
    property ButtonFinish: TJvWizardNavigateButton read FNavigateButtons[bkFinish] write FNavigateButtons[bkFinish];
    property ButtonCancel: TJvWizardNavigateButton read FNavigateButtons[bkCancel] write FNavigateButtons[bkCancel];
    property ButtonHelp: TJvWizardNavigateButton read FNavigateButtons[bkHelp] write FNavigateButtons[bkHelp];
    property ShowDivider: Boolean read FShowDivider write SetShowDivider default True;
    property ShowRouteMap: Boolean read GetShowRouteMap write SetShowRouteMap;
    property HeaderImages: TCustomImageList read FHeaderImages write SetHeaderImages;
    property OnSelectFirstPage: TJvWizardSelectPageEvent read FOnSelectFirstPage write FOnSelectFirstPage;
    property OnSelectLastPage: TJvWizardSelectPageEvent read FOnSelectLastPage write FOnSelectLastPage;
    property OnSelectNextPage: TJvWizardSelectPageEvent read FOnSelectNextPage write FOnSelectNextPage;
    property OnSelectPriorPage: TJvWizardSelectPageEvent read FOnSelectPriorPage write FOnSelectPriorPage;

    // (ahuser) BCB cannot handle enum types as index.
    property OnStartButtonClick: TNotifyEvent index Integer(bkStart) read GetButtonClick write SetButtonClick;
    property OnLastButtonClick: TNotifyEvent index Integer(bkLast) read GetButtonClick write SetButtonClick;
    property OnBackButtonClick: TNotifyEvent index Integer(bkBack) read GetButtonClick write SetButtonClick;
    property OnNextButtonClick: TNotifyEvent index Integer(bkNext) read GetButtonClick write SetButtonClick;
    property OnFinishButtonClick: TNotifyEvent index Integer(bkFinish) read GetButtonClick write SetButtonClick;
    property OnCancelButtonClick: TNotifyEvent index Integer(bkCancel) read GetButtonClick write SetButtonClick;
    property OnHelpButtonClick: TNotifyEvent index Integer(bkHelp) read GetButtonClick write SetButtonClick;

    property OnActivePageChanged: TNotifyEvent read FOnActivePageChanged write FOnActivePageChanged;
    property OnActivePageChanging: TJvWizardChangingPageEvent read FOnActivePageChanging write FOnActivePageChanging;

    property Color;
    property Font;
    property Enabled;
    property Visible;
  end;

implementation

uses
  {$IFDEF USEJVCL}
  JvResources,
  {$ENDIF USEJVCL}
  Consts;

const
  ciButtonWidth = 75;
  ciButtonHeight = 25;
  ciButtonBarHeight = 42;
  ciButtonPlacement = (ciButtonBarHeight - ciButtonHeight) div 2;

{$IFNDEF USEJVCL}
resourcestring
  RsBackButtonCaption = '< &Back';
  RsNextButtonCaption = '&Next >';

  RsFirstButtonCaption = 'To &Start Page';
  RsLastButtonCaption = 'To &Last Page';
  RsFinishButtonCaption = '&Finish';
  RsWelcome = 'Welcome';
  RsTitle = 'Title';
  RsSubtitle = 'Subtitle';

  RsEInvalidParentControl = 'The Parent should be TJvWizard or a descendant';
  RsEInvalidWizardPage = 'The pages belong to another wizard';
{$ENDIF USEJVCL}

type
  // (ahuser) introduced for refactoring the WizardButtons
  TJvWizardBaseButton = class(TJvWizardButtonControl)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); virtual;
    procedure SelectPage; virtual;
  public
    procedure Click; override;
  end;

  { YW - First Button }
  TJvWizardStartButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    procedure SelectPage; override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { YW - Last Button }
  TJvWizardLastButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    procedure SelectPage; override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { YW - Back Button }
  TJvWizardBackButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    procedure SelectPage; override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { YW - Next Button }
  TJvWizardNextButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    procedure SelectPage; override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { YW - Finish Button }
  TJvWizardFinishButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { YW - Cancel Button }
  TJvWizardCancelButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { YW - Help Button }
  TJvWizardHelpButton = class(TJvWizardBaseButton)
  protected
    procedure ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean); override;
    function StoreCaption: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

//=== TJvWizardButtonControl =================================================

constructor TJvWizardButtonControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then
  begin
    {$IFDEF COMPILER6_UP}
      { !!! YW - Add csClickEvents in order to fired the Click method
         at design time. It does NOT need at run time, otherwise it cause
        the OnClick event to be called twice. }
    ControlStyle := ControlStyle + [csClickEvents];
    {$ENDIF COMPILER6_UP}
    ControlStyle := ControlStyle + [csNoDesignVisible];
  end;
  Kind := bkCustom;
  Anchors := [akRight, akBottom];
end;

function TJvWizardButtonControl.StoreCaption: Boolean;
begin
  Result := True;
end;

{$IFDEF VCL}
procedure TJvWizardButtonControl.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  inherited;
  if Enabled then
    Msg.Result := 1;
end;
{$ENDIF VCL}

{$IFDEF VCL}
procedure TJvWizardButtonControl.CMVisibleChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FWizard) then
    FWizard.RepositionButtons;
end;
{$ENDIF VCL}
{$IFDEF VisualCLX}
procedure TJvWizardButtonControl.VisibleChanged;
begin
  inherited VisibleChanged;
  if Assigned(FWizard) then
    FWizard.RepositionButtons;
end;
{$ENDIF VisualCLX}

//=== TJvWizardBaseButton ====================================================

procedure TJvWizardBaseButton.Click;
var
  Stop: Boolean;
  Page: TJvWizardCustomPage;
begin
  if Assigned(FWizard) then
  begin
    if not (csDesigning in ComponentState) then
    begin
      Stop := False;
      Page := FWizard.FActivePage;
      if Assigned(Page) then
        ButtonClick(Page, Stop);
      if Stop then
        Exit;
      inherited Click;
    end;
    SelectPage;
  end;
end;

procedure TJvWizardBaseButton.SelectPage;
begin
  // default action: nothing
end;

procedure TJvWizardBaseButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  // do nothing (make Delphi 5 compiler happy)
end;

//=== TJvWizardStartButton ===================================================

constructor TJvWizardStartButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsFirstButtonCaption;
  Visible := False;
  Anchors := [akLeft, akBottom];
  Width := ciButtonWidth + 10;
  Alignment := alLeft;
end;

procedure TJvWizardStartButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  inherited ButtonClick(Page, Stop);
  if Assigned(Page.FOnStartButtonClick) then
    Page.FOnStartButtonClick(Page, Stop);
end;

procedure TJvWizardStartButton.SelectPage;
begin
  FWizard.SelectFirstPage;
end;

function TJvWizardStartButton.StoreCaption: Boolean;
begin
  Result := Caption <> RsFirstButtonCaption;
end;

//=== TJvWizardLastButton ====================================================

constructor TJvWizardLastButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsLastButtonCaption;
  Visible := False;
  Anchors := [akLeft, akBottom];
  Width := ciButtonWidth + 10;
  Alignment := alLeft;
end;

procedure TJvWizardLastButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  if Assigned(Page.FOnLastButtonClick) then
    Page.FOnLastButtonClick(Page, Stop);
end;

procedure TJvWizardLastButton.SelectPage;
begin
  FWizard.SelectLastPage;
end;

function TJvWizardLastButton.StoreCaption: Boolean;
begin
  Result := Caption <> RsLastButtonCaption;
end;

//=== TJvWizardBackButton ====================================================

constructor TJvWizardBackButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsBackButtonCaption;
  Enabled := False;
  Visible := True;
  Width := ciButtonWidth;
  Alignment := alRight;
end;

procedure TJvWizardBackButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  if Assigned(Page.FOnBackButtonClick) then
    Page.FOnBackButtonClick(Page, Stop);
end;

procedure TJvWizardBackButton.SelectPage;
begin
  FWizard.SelectPriorPage;
end;

function TJvWizardBackButton.StoreCaption: Boolean;
begin
  Result := Caption <> RsBackButtonCaption;
end;

//=== TJvWizardNextButton ====================================================

constructor TJvWizardNextButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsNextButtonCaption;
  Enabled := False;
  Visible := True;
  Width := ciButtonWidth;
  Alignment := alRight;
end;

procedure TJvWizardNextButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  if Assigned(Page.FOnNextButtonClick) then
    Page.FOnNextButtonClick(Page, Stop);
end;

procedure TJvWizardNextButton.SelectPage;
begin
  FWizard.SelectNextPage;
end;

function TJvWizardNextButton.StoreCaption: Boolean;
begin
  Result := Caption <> RsNextButtonCaption;
end;

//=== TJvWizardFinishButton ==================================================

constructor TJvWizardFinishButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsFinishButtonCaption;
  Visible := False;
  Width := ciButtonWidth;
  Alignment := alRight;
end;

procedure TJvWizardFinishButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  if Assigned(Page.FOnFinishButtonClick) then
    Page.FOnFinishButtonClick(Page, Stop);
end;

function TJvWizardFinishButton.StoreCaption: Boolean;
begin
  Result := Caption <> RsFinishButtonCaption;
end;

//=== TJvWizardCancelButton ==================================================

constructor TJvWizardCancelButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := SCancelButton;
  Visible := True;
  Cancel := True;
  Width := ciButtonWidth;
  Alignment := alRight;
  ModalResult := mrCancel;
end;

procedure TJvWizardCancelButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  if Assigned(Page.FOnCancelButtonClick) then
    Page.FOnCancelButtonClick(Page, Stop);
end;

function TJvWizardCancelButton.StoreCaption: Boolean;
begin
  Result := Caption <> SCancelButton;
end;

//=== TJvWizardHelpButton ====================================================

constructor TJvWizardHelpButton.Create(AOwner: TComponent); // Added by Theodore
begin
  inherited Create(AOwner);
  Caption := SHelpButton;
  Visible := False;
  Anchors := [akLeft, akBottom];
  Width := ciButtonWidth;
  Alignment := alLeft;
end;

procedure TJvWizardHelpButton.Click; // Added by Theodore, Modified by Yu Wei
var
  ID: THelpContext;
begin
  ID := 0;
  if not (csDesigning in ComponentState) then
  begin
    if Assigned(FWizard) and Assigned(FWizard.ActivePage) then
    begin
      if Assigned(FWizard.ActivePage.OnHelpButtonClick) then // MF
      begin
        inherited Click;
        Exit;
      end;
      ID := FWizard.ActivePage.HelpContext;
    end
    else
      ID := GetParentForm(Self).HelpContext;
  end;
  if ID <> 0 then
  begin
    {$IFDEF VCL}
    Application.HelpContext(ID);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    Application.ContextHelp(ID);
    {$ENDIF VisualCLX}
  end;
end;

procedure TJvWizardHelpButton.ButtonClick(Page: TJvWizardCustomPage; var Stop: Boolean);
begin
  if Assigned(Page.FOnHelpButtonClick) then
    Page.FOnHelpButtonClick(Page, Stop);
end;

function TJvWizardHelpButton.StoreCaption: Boolean;
begin
  Result := Caption <> SHelpButton;
end;

//=== TJvWizardNavigateButton ================================================

function TJvWizardNavigateButton.GetCaption: string;
begin
  if Assigned(FControl) then
    Result := FControl.Caption
  else
    Result := '';
end;

function TJvWizardNavigateButton.GetGlyph: TBitmap;
begin
  if Assigned(FControl) then
    Result := FControl.Glyph
  else
    Result := nil;
end;

function TJvWizardNavigateButton.GetLayout: TButtonLayout;
begin
  if Assigned(FControl) then
    Result := FControl.Layout
  else
    Result := blGlyphLeft;
end;

function TJvWizardNavigateButton.GetNumGlyphs: Integer;
begin
  if Assigned(FControl) then
    Result := FControl.NumGlyphs
  else
    Result := 0;
end;

procedure TJvWizardNavigateButton.SetCaption(const Value: string);
begin
  if Assigned(FControl) then
  begin
    FControl.Caption := Value;
  end;
end;

procedure TJvWizardNavigateButton.SetGlyph(const Value: TBitmap);
begin
  if Assigned(FControl) then
    FControl.Glyph := Value;
end;

procedure TJvWizardNavigateButton.SetNumGlyphs(const Value: Integer);
begin
  if Assigned(FControl) then
    FControl.NumGlyphs := Value;
end;

procedure TJvWizardNavigateButton.SetLayout(const Value: TButtonLayout);
begin
  if Assigned(FControl) then
    FControl.Layout := Value;
end;

function TJvWizardNavigateButton.GetModalResult: TModalResult;
begin
  if Assigned(FControl) then
    Result := FControl.ModalResult
  else
    Result := mrNone;
end;

procedure TJvWizardNavigateButton.SetModalResult(const Value: TModalResult);
begin
  if Assigned(FControl) then
    FControl.ModalResult := Value;
end;

function TJvWizardNavigateButton.GetButtonWidth: Integer;
begin
  if Assigned(FControl) then
    Result := FControl.Width
  else
    Result := 0;
end;

procedure TJvWizardNavigateButton.SetButtonWidth(const Value: Integer);
begin
  if Assigned(FControl) and (FControl.Width <> Value) then
  begin
    FControl.Width := Value;
    if Assigned(FControl.FWizard) then
      FControl.FWizard.RepositionButtons;
  end;
end;

function TJvWizardNavigateButton.StoreCaption: Boolean;
begin
  Result := not Assigned(FControl) or FControl.StoreCaption;
end;

//=== TJvWizardRouteMapControl ===============================================

constructor TJvWizardRouteMapControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { !!! YW - Add csNoDesignVisible in order to make it visible and invisible
    at design Time. }
  if csDesigning in ComponentState then
    ControlStyle := ControlStyle + [csNoDesignVisible];
  FAlign := alLeft;
  inherited Align := alLeft;
  TabStop := False;
  Width := 145;
  Visible := True;
  FPages := TList.Create;
  {$IFDEF VCL}
  DoubleBuffered := True;
  {$ENDIF VCL}
end;

destructor TJvWizardRouteMapControl.Destroy;
begin
  if Assigned(Wizard) then
    Wizard.FRouteMap := nil;
  FPages.Free;
  inherited Destroy;
end;

procedure TJvWizardRouteMapControl.DoAddPage(const APage: TJvWizardCustomPage);
begin
  if Assigned(FWizard) then
  begin
    if Assigned(APage) and (FPages.IndexOf(APage) < 0) then
      FPages.Add(APage);
    WizardPageAdded(APage);
  end;
end;

procedure TJvWizardRouteMapControl.DoDeletePage(const APage: TJvWizardCustomPage);
var
  I: Integer;
begin
  if Assigned(FWizard) then
  begin
    if Assigned(APage) then
    begin
      I := FPages.Remove(APage);
      if FPageIndex = I then
        FPageIndex := -1;
    end;
    WizardPageDeleted(APage);
  end;
end;

procedure TJvWizardRouteMapControl.DoActivatePage(const APage: TJvWizardCustomPage);
begin
  if Assigned(FWizard) then
  begin
    if Assigned(APage) then
      FPageIndex := FPages.IndexOf(APage);
    WizardPageActivated(APage);
  end;
end;

procedure TJvWizardRouteMapControl.DoUpdatePage(const APage: TJvWizardCustomPage);
begin
  if Assigned(FWizard) then
    WizardPageUpdated(APage);
end;

procedure TJvWizardRouteMapControl.DoMovePage(
  const APage: TJvWizardCustomPage; const OldIndex: Integer);
begin
  if Assigned(FWizard) then
  begin
    if Assigned(APage) then
    begin
      FPages.Move(OldIndex, APage.PageIndex);
      if OldIndex = FPageIndex then
        FPageIndex := APage.PageIndex;
    end;
    WizardPageMoved(APage, OldIndex);
  end;
end;

procedure TJvWizardRouteMapControl.SetAlign(Value: TJvWizardAlign);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    inherited Align := FAlign;
  end;
end;

function TJvWizardRouteMapControl.GetPage(Index: Integer): TJvWizardCustomPage;
begin
  if (Index >= 0) and (Index < FPages.Count) then
    Result := TJvWizardCustomPage(FPages[Index])
  else
    Result := nil;
end;

function TJvWizardRouteMapControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TJvWizardRouteMapControl.SetPageIndex(Value: Integer);
begin
  if (FPageIndex <> Value) and (Value >= 0) and (Value < PageCount) then
  begin
    FPageIndex := Value;
    if Assigned(FWizard) and (Pages[FPageIndex].Wizard = FWizard) then
      FWizard.SetActivePage(Pages[FPageIndex]);
  end;
end;

{$IFDEF VCL}
procedure TJvWizardRouteMapControl.CMDesignHitTest(var Msg: TCMDesignHitTest);
begin
  if PageAtPos(Point(Msg.XPos, Msg.YPos)) <> nil then
    Msg.Result := 1
  else
    inherited;
end;
{$ENDIF VCL}

function TJvWizardRouteMapControl.PageAtPos(Pt: TPoint): TJvWizardCustomPage;
begin
  { YW - Return the page object at the particular point in the route
    map control. Return NIL if no page at this particular point. }
  Result := nil;
end;

procedure TJvWizardRouteMapControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APage: TJvWizardCustomPage;
begin
  if Button = mbLeft then
  begin
    APage := PageAtPos(Point(X, Y));
    if Assigned(APage) and ((csDesigning in ComponentState) or
      (APage.Enabled and APage.EnableJumpToPage)) then  // Nonn
    begin
      if APage.PageIndex = PageIndex + 1 then
        Wizard.SelectNextPage
      else
      if APage.PageIndex = PageIndex - 1 then
        Wizard.SelectPriorPage
      else
        PageIndex := APage.PageIndex;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

{$IFDEF VisualCLX}
procedure TJvWizardRouteMapControl.SetParent(const ParentA: TWidgetControl);
var
  I: Integer;
  AParent: TWidgetControl;
begin
  AParent:= ParentA;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvWizardRouteMapControl.SetParent(AParent: TWinControl);
var
  I: Integer;
begin
{$ENDIF VCL}
  if Assigned(AParent) then
  begin
    if not ((AParent is TJvWizard) or (AParent is TJvWizardCustomPage)) then
      raise EJvWizardError.CreateRes(@RsEInvalidParentControl);
    if AParent is TJvWizardCustomPage then
      AParent := TJvWizardCustomPage(AParent).Wizard;
  end;
  inherited SetParent(AParent);
  if Assigned(AParent) then
  begin
    FWizard := TJvWizard(AParent);
    FWizard.FRouteMap := Self;
    FPages.Clear;
    for I := 0 to FWizard.PageCount - 1 do
      FPages.Add(FWizard.FPages[I]);

    if Assigned(FWizard.FActivePage) then
      FPageIndex := FWizard.FActivePage.PageIndex
    else
      FPageIndex := -1;
  end;
end;

procedure TJvWizardRouteMapControl.WizardPageActivated(
  const APage: TJvWizardCustomPage);
begin
  { YW - Called after the page becomes the current active page of wizard. }
  Invalidate;
end;

procedure TJvWizardRouteMapControl.WizardPageAdded(
  const APage: TJvWizardCustomPage);
begin
  { YW - Called after the new page added into wizard. }
  Invalidate;
end;

procedure TJvWizardRouteMapControl.WizardPageDeleted(
  const APage: TJvWizardCustomPage);
begin
  { YW - Called after the page is removed from.
     Note: do NOT free this page. }
  Invalidate;
end;

procedure TJvWizardRouteMapControl.WizardPageMoved(
  const APage: TJvWizardCustomPage; const OldIndex: Integer);
begin
  { YW - Called after page is changed its page order. }
  Invalidate;
end;

procedure TJvWizardRouteMapControl.WizardPageUpdated(
  const APage: TJvWizardCustomPage);
begin
  { YW - Called when the page change its status or caption. }
  Invalidate;
end;

function TJvWizardRouteMapControl.CanDisplay(const APage: TJvWizardCustomPage): Boolean;
begin
  Result := Assigned(APage) and ((csDesigning in ComponentState) or APage.Enabled);
  if not (csDesigning in ComponentState) and Assigned(FOnDisplaying) then
    FOnDisplaying(Self, APage, Result);
end;

//=== TJvWizardImage =========================================================

constructor TJvWizardImage.Create;
begin
  FPicture := TPicture.Create;
  FPicture.OnChange := DoPictureChange;
  FAlignment := iaStretch;
  FLayout := ilStretch;
end;

destructor TJvWizardImage.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvWizardImage.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvWizardImage.PaintTo(const ACanvas: TCanvas; ARect: TRect);
begin
  if Assigned(FPicture.Graphic) then
    JvWizardDrawImage(ACanvas, FPicture.Graphic, ARect, FAlignment, FLayout);
end;

procedure TJvWizardImage.SetAlignment(Value: TJvWizardImageAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    DoChange;
  end;
end;

procedure TJvWizardImage.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  if FPicture.Graphic <> nil then
    FPicture.Graphic.Transparent := FTransparent;
end;

procedure TJvWizardImage.SetLayout(Value: TJvWizardImageLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    DoChange;
  end;
end;

function TJvWizardImage.GetTransparent: Boolean;
var
  AGraphic: TGraphic;
begin
  AGraphic := FPicture.Graphic;
  if Assigned(AGraphic) then
    Result := AGraphic.Transparent
  else
    Result := FTransparent;
end;

procedure TJvWizardImage.SetTransparent(Value: Boolean);
var
  AGraphic: TGraphic;
begin
  AGraphic := FPicture.Graphic;
  FTransparent := Value;
  if Assigned(AGraphic) and not
     ({$IFDEF VCL}(AGraphic is TMetaFile) or {$ENDIF VCL} (AGraphic is TIcon)) then
  begin
    AGraphic.Transparent := Value;
  end;
end;

procedure TJvWizardImage.DoPictureChange(Sender: TObject);
begin
  DoChange;
end;

//=== TJvWizardGraphicObject =================================================

constructor TJvWizardGraphicObject.Create;
begin
  FColor := clBtnFace;
  FVisible := True;
end;

procedure TJvWizardGraphicObject.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ColorChanged;
  end;
end;

procedure TJvWizardGraphicObject.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    VisibleChanged;
  end;
end;

procedure TJvWizardGraphicObject.ColorChanged;
begin
  DoChange;
end;

procedure TJvWizardGraphicObject.VisibleChanged;
begin
  DoChange;
end;

//=== TJvWizardPageTitle =====================================================

constructor TJvWizardPageTitle.Create;
begin
  inherited Create;
  FAnchorPlacement := 4;
  FIndent := 0;
  FAnchors := [akLeft, akTop];
  FAlignment := taLeftJustify;
  FFont := TFont.Create;
  Color := clNone; // YW - Transparent
end;

destructor TJvWizardPageTitle.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

procedure TJvWizardPageTitle.DoChange;
begin
  if Assigned(FWizardPageHeader) then
    FWizardPageHeader.DoChange;
end;

procedure TJvWizardPageTitle.SetWizardPageHeader(Value: TJvWizardPageHeader);
begin
  if FWizardPageHeader <> Value then
  begin
    FWizardPageHeader := Value;
    if Assigned(FWizardPageHeader) and
      Assigned(FWizardPageHeader.WizardPage) then
    begin
      AdjustFont(FWizardPageHeader.WizardPage.Font);
    end;
    FFont.OnChange := OnChange;
  end;
end;

function TJvWizardPageTitle.GetTextRect(const ACanvas: TCanvas;
  const ARect: TRect): TRect;
var
  ATextSize: TSize;
begin
  ATextSize := ACanvas.TextExtent(FText);
  Result := Bounds(ARect.Left, ARect.Top, ATextSize.cx, ATextSize.cy);
  if akLeft in FAnchors then
    OffsetRect(Result, FAnchorPlacement, 0);
  if akTop in FAnchors then
    OffsetRect(Result, 0, FAnchorPlacement);
  if akRight in FAnchors then
    Result.Right := ARect.Right - FAnchorPlacement;
  if akBottom in FAnchors then
    Result.Bottom := ARect.Bottom - FAnchorPlacement;
  InflateRect(Result, -FIndent, 0);
  if Result.Bottom > ARect.Bottom then
    Result.Bottom := ARect.Bottom;
  if Result.Right > ARect.Right then
    Result.Right := ARect.Right;
end;

procedure TJvWizardPageTitle.PaintTo(ACanvas: TCanvas; var ARect: TRect);
const
  Alignments: array[TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  ATextRect: TRect;
begin
  if FVisible and Assigned(FWizardPageHeader) and
    Assigned(FWizardPageHeader.WizardPage) then
  begin
    ACanvas.Font.Assign(FFont);
    ATextRect := GetTextRect(ACanvas, ARect);
    if FColor <> clNone then
    begin
      ACanvas.Brush.Style := bsSolid;
      ACanvas.Brush.Color := FColor;
      ACanvas.FillRect(ATextRect);
    end;
    with ACanvas do
    begin
      Brush.Style := bsClear;
      DrawText(ACanvas.Handle, PChar(FText), -1, ATextRect,
        DT_WORDBREAK + Alignments[FAlignment]);
      { YW - Draw outline at design time. }
      if csDesigning in FWizardPageHeader.WizardPage.ComponentState then
      begin
        Pen.Style := psDot;
        Pen.Mode := pmXor;
        Pen.Color := $00FFD8CE;
        Brush.Style := bsClear;
        Rectangle(ATextRect.Left, ATextRect.Top, ATextRect.Right,
          ATextRect.Bottom);
      end;
    end;
    ARect.Top := ATextRect.Bottom;
  end;
end;

procedure TJvWizardPageTitle.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageTitle.SetAnchors(Value: TAnchors);
begin
  if FAnchors <> Value then
  begin
    FAnchors := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageTitle.SetIndent(Value: Integer);
begin
  if FIndent <> Value then
  begin
    FIndent := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageTitle.SetFont(Value: TFont);
begin
  if (FFont <> Value) and
    not (Assigned(FWizardPageHeader) and FWizardPageHeader.ParentFont) then
  begin
    FFont.Assign(Value);
  end;
end;

procedure TJvWizardPageTitle.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    DoChange;
    if Assigned(FWizardPageHeader) and
      Assigned(FWizardPageHeader.WizardPage) and
      Assigned(FWizardPageHeader.WizardPage.Wizard) and
      Assigned(FWizardPageHeader.WizardPage.Wizard.FRouteMap) then
    begin
      FWizardPageHeader.WizardPage.Wizard.FRouteMap.DoUpdatePage(FWizardPageHeader.WizardPage);
    end;
  end;
end;

procedure TJvWizardPageTitle.SetAnchorPlacement(Value: Integer);
begin
  if FAnchorPlacement <> Value then
  begin
    FAnchorPlacement := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageTitle.AdjustFont(const AFont: TFont);
begin
  if Assigned(AFont) then
  begin
    FFont.Name := AFont.Name;
    FFont.Charset := AFont.Charset;
  end;
end;

procedure TJvWizardPageTitle.OnChange(Sender: TObject);
begin
  DoChange;
end;

//=== TJvWizardPageObject ====================================================

procedure TJvWizardPageObject.DoChange;
begin
  if Assigned(FWizardPage) then
    FWizardPage.Invalidate;
end;

procedure TJvWizardPageObject.Initialize;
begin
end;

procedure TJvWizardPageObject.SetWizardPage(Value: TJvWizardCustomPage);
begin
  FWizardPage := Value;
  Initialize;
end;

//=== TJvWizardPageHeader ====================================================

constructor TJvWizardPageHeader.Create;
begin
  inherited Create;
  Color := clWindow;
  FHeight := 70;
  FParentFont := True;
  { Set up Title }
  FTitle := TJvWizardPageTitle.Create;
  FTitle.FText := RsTitle;
  FTitle.FAnchors := [akLeft, akTop, akRight];
  FTitle.FFont.Size := 12;
  FTitle.FFont.Style := [fsBold];
  { Set up Subtitle }
  FSubtitle := TJvWizardPageTitle.Create;
  FSubtitle.FAnchors := [akLeft, akTop, akRight, akBottom];
  FSubtitle.FText := RsSubtitle;
  FImageAlignment := iaRight;
  FImageOffset := 0;
  FImageIndex := -1;
  FShowDivider := True;
end;

destructor TJvWizardPageHeader.Destroy;
begin
  FTitle.Free;
  FSubtitle.Free;
  inherited Destroy;
end;

procedure TJvWizardPageHeader.Initialize;
begin
  FTitle.WizardPageHeader := Self;
  FSubtitle.WizardPageHeader := Self;
end;

procedure TJvWizardPageHeader.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageHeader.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageHeader.SetImageOffset(Value: Integer);
begin
  if FImageOffset <> Value then
  begin
    FImageOffset := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageHeader.SetImageAlignment(Value: TJvWizardImageLeftRight);
begin
  if FImageAlignment <> Value then
  begin
    FImageAlignment := Value;
    DoChange;
  end;
end;

procedure TJvWizardPageHeader.SetShowDivider(Value: Boolean);
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    DoChange;
  end;
end;

function TJvWizardPageHeader.GetImageRect(const AImages: TCustomImageList;
  var ARect: TRect): TRect;
begin
  Result := Bounds(ARect.Left, ARect.Top, AImages.Width, AImages.Height);
  OffsetRect(Result, 0, ((ARect.Bottom - ARect.Top) - AImages.Height) div 2);
  if FImageAlignment = iaRight then
  begin
    OffsetRect(Result, ARect.Right - ARect.Left - AImages.Width - 4, 0);
  end;
  if FImageAlignment = iaLeft then
  begin
    OffsetRect(Result, FImageOffset, 0);
    { YW - if right side of the image area still in the page header area
      then adjust the left side of title area. }
    if Result.Right > ARect.Left then
      ARect.Left := Result.Right;
  end
  else // must be iaRight
  begin
    OffsetRect(Result, -FImageOffset, 0);
    { YW - if left side of the image area still in the page header area
      then adjust the ride side of title area. }
    if Result.Left < ARect.Right then
      ARect.Right := Result.Left;
  end;
end;

procedure TJvWizardPageHeader.PaintTo(ACanvas: TCanvas; var ARect: TRect);
var
  R, ImageRect: TRect;
  AImages: TCustomImageList;
begin
  if Visible then
  begin
    R := ARect;
    R.Bottom := R.Top + FHeight;
    with ACanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(R);
    end;
    if Assigned(WizardPage) then
    begin
      { YW - Show Header Divider }
      if {(csDesigning in WizardPage.ComponentState) or} FShowDivider then
      begin
        JvWizardDrawBorderEdges(ACanvas, R, fsGroove, [beBottom]);
      end;
      { YW - Draw Header Image first }
      if Assigned(WizardPage.Wizard) then
      begin
        AImages := WizardPage.Wizard.HeaderImages;
        if Assigned(AImages) and (FImageIndex >= 0) and
          (FImageIndex < AImages.Count) then
        begin
          ImageRect := GetImageRect(AImages, R);
          { YW - R is the area where the title and subtitle paint to. }
          AImages.Draw(ACanvas, ImageRect.Left, ImageRect.Top, FImageIndex {$IFDEF VCL}, True {$ENDIF});
        end;
      end;
      { YW - Draw Title }
      FTitle.PaintTo(ACanvas, R);
      { YW - Draw Subtitle }
      FSubtitle.PaintTo(ACanvas, R);
    end;
    Inc(ARect.Top, FHeight);
  end;
end;

procedure TJvWizardPageHeader.AdjustTitleFont;
begin
  if Assigned(FWizardPage) and FParentFont then
  begin
    if Assigned(FTitle) then
    begin
      FTitle.AdjustFont(FWizardPage.Font);
    end;
    if Assigned(FSubtitle) then
    begin
      FSubtitle.AdjustFont(FWizardPage.Font);
    end;
  end;
end;

procedure TJvWizardPageHeader.SetParentFont(Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    AdjustTitleFont;
  end;
end;

procedure TJvWizardPageHeader.VisibleChanged;
begin
  inherited VisibleChanged; // !!!
  if Assigned(WizardPage) then
    WizardPage.Realign;
end;

//=== TJvWizardWaterMark =====================================================

constructor TJvWizardWaterMark.Create;
begin
  inherited Create;
  FAlign := alLeft;
  {$IFDEF VCL}
  Color := clActiveCaption;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Color := clActiveHighlight;
  {$ENDIF VisualCLX}
  FWidth := 164;
  FBorderWidth := 1;
  FImage := TJvWizardImage.Create;
  FImage.OnChange := ImageChanged;
end;

destructor TJvWizardWaterMark.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;

procedure TJvWizardWaterMark.SetBorderWidth(Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    DoChange;
  end;
end;

procedure TJvWizardWaterMark.SetAlign(Value: TJvWizardLeftRight);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    DoChange;
    if Assigned(WizardPage) then
    begin
      WizardPage.Realign;
    end;
  end;
end;

procedure TJvWizardWaterMark.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    DoChange;
  end;
end;

procedure TJvWizardWaterMark.PaintTo(ACanvas: TCanvas; var ARect: TRect);
var
  R: TRect;
  AHeight: Integer;
begin
  if Visible then
  begin
    AHeight := ARect.Bottom - ARect.Top;
    if FAlign = alLeft then
    begin
      R := Bounds(ARect.Left, ARect.Top, FWidth, AHeight);
      Inc(ARect.Left, FWidth);
    end
    else // YW - must be alRight
    begin
      R := Bounds(ARect.Right - FWidth, ARect.Top, FWidth, AHeight);
      Dec(ARect.Right, FWidth);
    end;
    InflateRect(R, -FBorderWidth, -FBorderWidth);
    with ACanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(R);
    end;
    FImage.PaintTo(ACanvas, R);
  end;
end;

procedure TJvWizardWaterMark.ImageChanged(Sender: TObject);
begin
  DoChange;
end;

procedure TJvWizardWaterMark.VisibleChanged;
begin
  inherited VisibleChanged; // !!!
  if Assigned(WizardPage) then
    WizardPage.Realign;
end;

//=== TJvWizardPagePanel =====================================================

constructor TJvWizardPagePanel.Create;
begin
  inherited Create;
  FBorderWidth := 7;
  Color := clBtnFace;
  Visible := False;
end;

procedure TJvWizardPagePanel.PaintTo(ACanvas: TCanvas; var ARect: TRect);
begin
  if Visible and (FBorderWidth > 0) then
  begin
    InflateRect(ARect, -FBorderWidth, -FBorderWidth);
    JvWizardDrawBorderEdges(ACanvas, ARect, fsGroove, beAllEdges);
    if Color <> clNone then // YW - clNone means transparent
    begin
      InflateRect(ARect, -2, -2);
      with ACanvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := Color;
        FillRect(ARect);
      end;
    end;
  end;
end;

procedure TJvWizardPagePanel.SetBorderWidth(Value: Word);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    DoChange;
  end;
end;

//=== TJvWizardCustomPage ====================================================

constructor TJvWizardCustomPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Visible := False;
  Color := clBtnFace;
  FHeader := TJvWizardPageHeader.Create;
  FHeader.WizardPage := Self;
  FImage := TJvWizardImage.Create;
  FImage.OnChange := ImageChanged;
  FPanel := TJvWizardPagePanel.Create;
  FPanel.WizardPage := Self;
  { YW - try to avoid screen flicker, it paints its image
    into memory, then move image memory to the screen at once. }
  FEnabledButtons := bkAllButtons;
  FVisibleButtons := [bkBack, bkNext, bkCancel];
  {$IFDEF VCL}
  DoubleBuffered := True;
  {$ENDIF VCL}
  FEnableJumpToPage := True; // Nonn
end;

destructor TJvWizardCustomPage.Destroy;
begin
  if Assigned(FWizard) then
    FWizard.RemovePage(Self);
  FPanel.Free;
  FImage.Free;
  FHeader.Free;
  inherited Destroy;
end;

{$IFDEF VCL}
procedure TJvWizardCustomPage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do
    Style := Style and not (CS_HREDRAW or CS_VREDRAW);
end;
{$ENDIF VCL}

procedure TJvWizardCustomPage.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if FHeader.Visible then
    Rect.Top := Rect.Top + FHeader.Height;
end;

procedure TJvWizardCustomPage.EnableButton(AButton: TJvWizardButtonKind; AEnabled: boolean); // Arioch
var
  IsEnabled: boolean;
  tmpSet: TJvWizardButtonSet;
begin
  tmpSet := [AButton];
  IsEnabled := (tmpSet * EnabledButtons) <> [];
  if AEnabled = IsEnabled then
    System.Exit;

  if AEnabled then
    EnabledButtons := EnabledButtons + tmpSet
  else
    EnabledButtons := EnabledButtons - tmpSet;
end;

{$IFDEF VisualCLX}
procedure TJvWizardCustomPage.EnabledChanged;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvWizardCustomPage.CMEnabledChanged(var Msg: TMessage);
{$ENDIF VCL}
var
  NextPage: TJvWizardCustomPage;
begin
  inherited;
  if Assigned(FWizard) then
  begin
    if Assigned(FWizard.FRouteMap) then
      FWizard.FRouteMap.DoUpdatePage(Self);
    if not ((csDesigning in ComponentState) or Enabled) and
      (FWizard.ActivePage = Self) then
    begin
      NextPage := FWizard.FindNextPage(PageIndex, 1,
        not (csDesigning in ComponentState));
      if not Assigned(NextPage) then
      begin
        NextPage := FWizard.FindNextPage(PageIndex, -1,
          not (csDesigning in ComponentState));
      end;
      FWizard.SetActivePage(NextPage);
    end;
  end;
end;

{$IFDEF VisualCLX}
procedure TJvWizardCustomPage.TextChanged;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvWizardCustomPage.CMTextChanged(var Msg: TMessage);
{$ENDIF VCL}
begin
  Invalidate;
  if Assigned(FWizard) and Assigned(FWizard.FRouteMap) then
    FWizard.FRouteMap.DoUpdatePage(Self);
end;

{$IFDEF VisualCLX}
procedure TJvWizardCustomPage.FontChanged;
begin
  FHeader.AdjustTitleFont;
  inherited FontChanged;
end;
{$ENDIF VisualCLX}
{$IFDEF VCL}
procedure TJvWizardCustomPage.CMFontChanged(var Msg: TMessage);
begin
  FHeader.AdjustTitleFont;
  inherited;
end;
{$ENDIF VCL}

procedure TJvWizardCustomPage.SetWizard(AWizard: TJvWizard);
begin
  if FWizard <> AWizard then
  begin
    if Assigned(FWizard) then
      FWizard.RemovePage(Self);
    Parent := AWizard;
    if Assigned(AWizard) then
      AWizard.InsertPage(Self);
  end;
end;

function TJvWizardCustomPage.GetPageIndex: Integer;
begin
  if Assigned(FWizard) then
    Result := FWizard.FPages.IndexOf(Self)
  else
    Result := -1;
end;

procedure TJvWizardCustomPage.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TJvWizard then
    Wizard := TJvWizard(Reader.Parent);
end;

procedure TJvWizardCustomPage.SetPageIndex(const Value: Integer);
var
  OldIndex: Integer;
begin
  if Assigned(FWizard) and (Value >= 0) and (Value < FWizard.FPages.Count) then
  begin
    OldIndex := PageIndex;
    FWizard.FPages.Move(OldIndex, Value);
    if Assigned(FWizard.FRouteMap) then
      FWizard.FRouteMap.DoMovePage(Self, OldIndex);
  end;
end;

{$IFDEF VCL}
procedure TJvWizardCustomPage.WMEraseBkgnd(var Msg: TWmEraseBkgnd);
begin
  Msg.Result := 1;
end;
{$ENDIF VCL}

procedure TJvWizardCustomPage.Paint;
var
  ARect: TRect;
begin
  if FDrawing then
    System.Exit;
  FDrawing := True;
  try
    ARect := ClientRect;
    with Canvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(ARect);
    end;
    DrawPage(Canvas, ARect);
    if Assigned(FOnPaintPage) and not (csDesigning in ComponentState) then
      FOnPaintPage(Self, Canvas, ARect)
    else
    begin
      { YW - Paint the image first to prevent the image from covering
        the panel. }
      FImage.PaintTo(Canvas, ARect);
      FPanel.PaintTo(Canvas, ARect);
    end;
    { YW - display page caption at design time. }
    if csDesigning in ComponentState then
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Assign(Font);
      {$IFDEF VCL}
      DrawText(Canvas.Handle, PChar(Caption), -1, ARect,
        DT_SINGLELINE + DT_CENTER + DT_VCENTER);
      {$ENDIF VCL}
      {$IFDEF VisualCLX}
      DrawTextW(Canvas.Handle, PWideChar(Caption), -1, ARect,
        DT_SINGLELINE + DT_CENTER + DT_VCENTER);
      {$ENDIF VisualCLX}
    end;
  finally
    FDrawing := False;
  end;
end;

procedure TJvWizardCustomPage.DrawPage(ACanvas: TCanvas; var ARect: TRect);
begin
  { YW - all derived page control should call this method to paint itsself
    rather than call the overrided paint method. }
end;

procedure TJvWizardCustomPage.Done;
begin
  Refresh; // !!! YW - Force the page to repaint itself immediately.
  if Assigned(FOnPage) and Enabled and not (csDesigning in ComponentState) then
    FOnPage(Self);
end;

procedure TJvWizardCustomPage.Enter(const FromPage: TJvWizardCustomPage);
begin
  if Assigned(FOnEnterPage) and Enabled and
    not (csDesigning in ComponentState) then
  begin
    FOnEnterPage(Self, FromPage);
  end;
end;

procedure TJvWizardCustomPage.Exit(const ToPage: TJvWizardCustomPage);
begin
  if Assigned(FOnExitPage) and Enabled and
    not (csDesigning in ComponentState) then
  begin
    FOnExitPage(Self, ToPage);
  end;
end;

procedure TJvWizardCustomPage.ImageChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvWizardCustomPage.SetEnabledButtons(Value: TJvWizardButtonSet);
begin
  if FEnabledButtons <> Value then
  begin
    FEnabledButtons := Value;
    if Assigned(FWizard) and (FWizard.FActivePage = Self) then
      FWizard.UpdateButtonsStatus;
  end;
end;

procedure TJvWizardCustomPage.SetVisibleButtons(Value: TJvWizardButtonSet);
begin
  if FVisibleButtons <> Value then
  begin
    FVisibleButtons := Value;
    if Assigned(FWizard) and (FWizard.FActivePage = Self) then
    begin
      { YW - if there is no buttons are visible, then we don't need
        to display the button bar. }
      if FWizard.AutoHideButtonBar then
      begin
        if FVisibleButtons = [] then
          FWizard.ButtonBarHeight := 0
        else
          FWizard.ButtonBarHeight := ciButtonBarHeight;
      end;
      Invalidate;
    end;
  end;
end;

function TJvWizardCustomPage.GetSubtitle: TJvWizardPageTitle;
begin
  Result := Header.Subtitle;
end;

function TJvWizardCustomPage.GetTitle: TJvWizardPageTitle;
begin
  Result := Header.Title;
end;

procedure TJvWizardCustomPage.SetSubtitle(const Value: TJvWizardPageTitle);
begin
  Header.Subtitle := Value;
end;

procedure TJvWizardCustomPage.SetTitle(const Value: TJvWizardPageTitle);
begin
  Header.Title := Value;
end;

//=== TJvWizardWelcomePage ===================================================

constructor TJvWizardWelcomePage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWaterMark := TJvWizardWaterMark.Create;
  FWaterMark.WizardPage := Self;
  FHeader.FTitle.FText := RsWelcome;
  // welcome pages don't have dividers by default
//  FHeader.ShowDivider := False;
  Color := clWindow;
end;

destructor TJvWizardWelcomePage.Destroy;
begin
  FWaterMark.Free;
  inherited Destroy;
end;

procedure TJvWizardWelcomePage.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect); // !!! YW - must call
  if FWaterMark.Visible then
  begin
    if FWaterMark.Align = alLeft then
      Rect.Left := Rect.Left + FWaterMark.Width
    else
      Rect.Right := Rect.Right - FWaterMark.Width;
  end;
end;

procedure TJvWizardWelcomePage.DrawPage(ACanvas: TCanvas; var ARect: TRect);
begin
  FWaterMark.PaintTo(ACanvas, ARect);
  FHeader.PaintTo(ACanvas, ARect);
end;

//=== TJvWizardInteriorPage ==================================================

procedure TJvWizardInteriorPage.DrawPage(ACanvas: TCanvas; var ARect: TRect);
begin
  FHeader.PaintTo(ACanvas, ARect);
end;

//=== TJvWizardPageList ======================================================

destructor TJvWizardPageList.Destroy;
begin
  FWizard := nil;
  inherited Destroy;
end;

function TJvWizardPageList.GetItems(Index: Integer): TJvWizardCustomPage;
begin
  Result := TJvWizardCustomPage(inherited Items[Index]);
end;

procedure TJvWizardPageList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  case Action of
    lnAdded:
      TJvWizardCustomPage(Ptr).FWizard := FWizard;
    lnDeleted:
      TJvWizardCustomPage(Ptr).FWizard := nil;
  end;
end;

//=== TJvWizard ==============================================================

constructor TJvWizard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { YW - In order to accept TJvWizardRouteMap control, we need to add
    csAcceptsControls ControlStyle }
  ControlStyle := ControlStyle + [csAcceptsControls];
  FPages := TJvWizardPageList.Create;
  FPages.Wizard := Self;
  Align := alClient;
  FShowDivider := True;
  FButtonBarHeight := ciButtonBarHeight;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FAutoHideButtonBar := True;
  CreateNavigateButtons;
  {$IFDEF VisualCLX}
  InputKeys := [ikAll];
  {$ENDIF VisualCLX}
end;

destructor TJvWizard.Destroy;
var
  I: Integer;
begin
  DestroyNavigateButtons;
  { !!! YW - Reset wizard property value of all wizard pages FIRST,
    so that when the actual wizard page control is freed, the page won't
    call Wizard.RemovePage, otherwise it will cause AV, because at that
    time FPages has already been destroyed. }
  for I := 0 to FPages.Count - 1 do
    TJvWizardCustomPage(FPages[I]).FWizard := nil;
  FImageChangeLink.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure TJvWizard.Loaded;
begin
  inherited Loaded;
  RepositionButtons;
  { YW - When the wizard shows up, by default we display the first page. }
  if FPages.Count > 0 then
    SelectFirstPage;
end;

function TJvWizard.GetButtonControlClass(AKind: TJvWizardButtonKind): TJvWizardButtonControlClass;
begin
  case AKind of
    bkStart:
      Result := TJvWizardStartButton;
    bkLast:
      Result := TJvWizardLastButton;
    bkBack:
      Result := TJvWizardBackButton;
    bkNext:
      Result := TJvWizardNextButton;
    bkFinish:
      Result := TJvWizardFinishButton;
    bkCancel:
      Result := TJvWizardCancelButton;
    bkHelp:
      Result := TJvWizardHelpButton;
  else
    Result := TJvWizardButtonControl;
  end;
end;

procedure TJvWizard.CreateNavigateButtons;
var
  AKind: TJvWizardButtonKind;
  AButton: TJvWizardButtonControl;
begin
  for AKind := Low(TJvWizardButtonKind) to High(TJvWizardButtonKind) do
  begin
    { YW - Don't need to set width property }
    AButton := GetButtonControlClass(AKind).Create(Self);
    try
      AButton.Parent := Self;
      AButton.Height := ciButtonHeight;
      AButton.Wizard := Self;
    finally
      FNavigateButtons[AKind] := TJvWizardNavigateButton.Create;
      FNavigateButtons[AKind].Control := AButton;
    end;
  end;
end;

procedure TJvWizard.DestroyNavigateButtons;
var
  AKind: TJvWizardButtonKind;
begin
  for AKind := Low(TJvWizardButtonKind) to High(TJvWizardButtonKind) do
    FNavigateButtons[AKind].Free;
end;

function TJvWizard.FindNextPage(PageIndex: Integer; const Step: Integer = 1;
  CheckDisable: Boolean = True): TJvWizardCustomPage;
begin
  { !!! YW - Only the Enabled property of the page can tell if it should be
     ignore or skip. we can not use Visible property, because all pages are
     invisible at startup until they are actived. }
  Result := nil;
  Assert(Step <> 0);
  repeat
    Inc(PageIndex, Step);
  until (PageIndex < 0) or (PageIndex >= FPages.Count) or
    TJvWizardCustomPage(FPages[PageIndex]).Enabled or not CheckDisable;
  if csDesigning in ComponentState then
  begin
    if PageIndex < 0 then
      PageIndex := FPages.Count - 1
    else
    if PageIndex >= FPages.Count then
      PageIndex := 0;
  end;
  if (PageIndex >= 0) and (PageIndex < FPages.Count) and
    (TJvWizardCustomPage(FPages[PageIndex]).Enabled or not CheckDisable) then
    Result := TJvWizardCustomPage(FPages[PageIndex]);
end;

// Nonn ...
function TJvWizard.FindNextEnabledPage(PageIndex: Integer; const Step: Integer = 1;
  CheckDisable: Boolean = True): TJvWizardCustomPage;
var
  APage: TJvWizardCustomPage;
begin
  APage := FindNextPage(PageIndex, Step, CheckDisable);
  if Assigned(APage) and not APage.EnableJumpToPage then
    APage := FindNextPage(APage.PageIndex, Step, CheckDisable);
  Result := APage;
end;
// ...Nonn

procedure TJvWizard.SelectFirstPage;
var
  AFirstPage: TJvWizardCustomPage;
begin
  // Nonn AFirstPage := FindNextPage(-1, 1, not (csDesigning in ComponentState));
  AFirstPage := FindNextEnabledPage(-1, 1, not (csDesigning in ComponentState));  // Nonn
  if Assigned(AFirstPage) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FOnSelectFirstPage) then
      FOnSelectFirstPage(Self, FActivePage, AFirstPage);
    if Assigned(AFirstPage) then
      SetActivePage(AFirstPage);
  end;
end;

procedure TJvWizard.SelectLastPage;
var
  ALastPage: TJvWizardCustomPage;
begin
// Nonn ALastPage := FindNextPage(FPages.Count, -1,
  ALastPage := FindNextEnabledPage(FPages.Count, -1,    // Nonn
    not (csDesigning in ComponentState));
  if Assigned(ALastPage) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FOnSelectLastPage) then
      FOnSelectLastPage(Self, FActivePage, ALastPage);
    if Assigned(ALastPage) then
      SetActivePage(ALastPage);
  end;
end;

procedure TJvWizard.SelectNextPage;
var
  ANextPage: TJvWizardCustomPage;
begin
// Nonn  ANextPage := FindNextPage(GetActivePageIndex, 1,
  ANextPage := FindNextEnabledPage(GetActivePageIndex, 1,  // Nonn
    not (csDesigning in ComponentState));
  if Assigned(ANextPage) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FOnSelectNextPage) then
      FOnSelectNextPage(Self, FActivePage, ANextPage);
    if Assigned(ANextPage) then
      SetActivePage(ANextPage);
  end;
end;

procedure TJvWizard.SelectPriorPage;
var
  APriorPage: TJvWizardCustomPage;
begin
// Nonn  APriorPage := FindNextPage(GetActivePageIndex, -1,
  APriorPage := FindNextEnabledPage(GetActivePageIndex, -1,  // Nonn
    not (csDesigning in ComponentState));
  if Assigned(APriorPage) then
  begin
    if not (csDesigning in ComponentState) and Assigned(FOnSelectPriorPage) then
      FOnSelectPriorPage(Self, FActivePage, APriorPage);
    if Assigned(APriorPage) then
      SetActivePage(APriorPage);
  end;
end;

function TJvWizard.IsFirstPage(const APage: TJvWizardCustomPage;
  CheckDisable: Boolean = True): Boolean;
var
  AFirstPage: TJvWizardCustomPage;
begin
  AFirstPage := FindNextPage(-1, 1, CheckDisable);
  Result := not Assigned(AFirstPage) or (APage = AFirstPage);
end;

function TJvWizard.IsLastPage(const APage: TJvWizardCustomPage;
  CheckDisable: Boolean = True): Boolean;
var
  ALastPage: TJvWizardCustomPage;
begin
  ALastPage := FindNextPage(FPages.Count, -1, CheckDisable);
  Result := not Assigned(ALastPage) or (APage = ALastPage);
end;

procedure TJvWizard.SetActivePage(Page: TJvWizardCustomPage);
begin
  if not (csLoading in ComponentState) and
    (not Assigned(Page) or ((Page.Wizard = Self) and
    ((csDesigning in ComponentState) or Page.Enabled))) then
  begin
    ChangeActivePage(Page);
  end;
end;

procedure TJvWizard.ChangeActivePage(Page: TJvWizardCustomPage);
var
  ParentForm: TCustomForm;
begin
  if FActivePage <> Page then
  begin
    DoActivePageChanging(Page);
    if Page = FActivePage then
      Exit;

    ParentForm := GetParentForm(Self);
    if Assigned(ParentForm) and Assigned(FActivePage) and
      FActivePage.ContainsControl(ParentForm.ActiveControl) and FActivePage.CanFocus then
    begin
      ParentForm.ActiveControl := FActivePage;
    end;

    if Assigned(FActivePage) then
    begin
      { YW - FActivePage.Exit, called just before the page is hidden. }
      FActivePage.Exit(Page);
      FActivePage.Visible := False;
    end;

    { YW - Just in case the new page is changed to be disabled again after
      the above OnExitPage event is called. }
    if Assigned(Page) and not (Page.Enabled or (csDesigning in ComponentState)) then
    begin
      if IsForward(FActivePage, Page) then
        Page := FindNextPage(GetActivePageIndex) // try go forward
      else
        Page := FindNextPage(GetActivePageIndex, -1); // try go backward
    end;

    if Assigned(Page) then
    begin
      { YW - FActivePage.Enter, called before the page shows up. }
      Page.Enter(FActivePage);
      Page.BringToFront;
      Page.Visible := True;
      if Assigned(ParentForm) then
      begin
        if Page.CanFocus then
          ParentForm.ActiveControl := Page
        else
        if CanFocus then
          ParentForm.ActiveControl := Self;
      end;
    end;

    FActivePage := Page;
    if Assigned(FRouteMap) then
      FRouteMap.DoActivatePage(FActivePage);
    if AutoHideButtonBar then
    begin
      if Assigned(FActivePage) and (FActivePage.FVisibleButtons = []) then
        ButtonBarHeight := 0
      else
        ButtonBarHeight := ciButtonBarHeight;
    end;
    { YW - At design time, if the Page's Enabled property set to False,
      the following if block never gets called. }
    if Assigned(ParentForm) and Assigned(FActivePage) and
      (ParentForm.ActiveControl = FActivePage) then
    begin
      FActivePage.SelectFirst;
      FActivePage.Done;
    end;

    DoActivePageChanged;
  end;
end;

procedure TJvWizard.InsertPage(Page: TJvWizardCustomPage);
begin
  FPages.Add(Page);
  if Assigned(FRouteMap) then
    FRouteMap.DoAddPage(Page);
end;

procedure TJvWizard.RemovePage(Page: TJvWizardCustomPage);
var
  NextPage: TJvWizardCustomPage;
begin
  if ActivePage = Page then
    NextPage := FindNextPage(Page.PageIndex, 1, not (csDesigning in ComponentState))
  else
    NextPage := ActivePage;
    
  if NextPage = Page then
    NextPage := nil;
  if Assigned(FRouteMap) then
    FRouteMap.DoDeletePage(Page);
  FPages.Remove(Page);
  SetActivePage(NextPage);
  { !!! YW - We must not call Page.Free, becaure page is the child
    control of the wizard now, so when the wizard being destroy, this page
    will be destroyed as well. }
end;

function TJvWizard.GetActivePageIndex: Integer;
begin
  if Assigned(ActivePage) then
    Result := ActivePage.PageIndex
  else
    Result := -1;
end;

procedure TJvWizard.SetActivePageIndex(Value: Integer);
begin
  if (Value >= 0) and (Value < PageCount) then
    ActivePage := TJvWizardCustomPage(FPages[Value])
  else
    ActivePage := nil;
end;

{$IFDEF VCL}
procedure TJvWizard.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;
{$ENDIF VCL}

procedure TJvWizard.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  if Color <> clNone then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(R);
  end;
  if (FButtonBarHeight > ciButtonHeight) and FShowDivider then
  begin
    R.Top := R.Bottom - FButtonBarHeight;
    JvWizardDrawBorderEdges(Canvas, R, fsGroove, [beTop]);
  end;
end;

procedure TJvWizard.SetShowDivider(Value: Boolean);
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    Invalidate;
  end;
end;

function TJvWizard.GetShowRouteMap: Boolean;
begin
  if Assigned(FRouteMap) then
    Result := FRouteMap.Visible
  else
    Result := False;
end;

procedure TJvWizard.SetShowRouteMap(Value: Boolean);
begin
  if Assigned(FRouteMap) then
    FRouteMap.Visible := Value;
end;

procedure TJvWizard.SetButtonBarHeight(Value: Integer);
begin
  if FButtonBarHeight <> Value then
  begin
    FButtonBarHeight := Value;
    Realign;
    RepositionButtons;
    Invalidate;
  end;
  { YW - Whatever the ButtonBarHeight is changed or not, we need to
     call UpdateButtonsStatus method anyway. }
  UpdateButtonsStatus;
end;

procedure TJvWizard.SetHeaderImages(Value: TCustomImageList);
begin
  if Assigned(FHeaderImages) then
    FHeaderImages.UnRegisterChanges(FImageChangeLink);
  FHeaderImages := Value;
  if Assigned(FHeaderImages) then
  begin
    FHeaderImages.RegisterChanges(FImageChangeLink);
    FHeaderImages.FreeNotification(Self);
  end;
  if Assigned(FActivePage) then
    FActivePage.Invalidate;
end;

function TJvWizard.GetButtonClick(Index: Integer): TNotifyEvent;
begin
  if Assigned(FNavigateButtons[TJvWizardButtonKind(Index)].Control) then
    Result := FNavigateButtons[TJvWizardButtonKind(Index)].Control.OnClick
  else
    Result := nil;
end;

procedure TJvWizard.SetButtonClick(Index: Integer; const Value: TNotifyEvent);
begin
  if Assigned(FNavigateButtons[TJvWizardButtonKind(Index)].Control) then
    FNavigateButtons[TJvWizardButtonKind(Index)].Control.OnClick := Value;
end;

function TJvWizard.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

{$IFDEF VCL}
procedure TJvWizard.CMDesignHitTest(var Msg: TCMDesignHitTest);
var
  Pt: TPoint;
begin
  Pt := SmallPointToPoint(Msg.Pos);
  if Assigned(FActivePage) and PtInRect(FActivePage.BoundsRect, Pt) then
    Msg.Result := 1;
end;
{$ENDIF VCL}

procedure TJvWizard.AdjustClientRect(var Rect: TRect);
begin
  { YW - All wizard's child controls (Pages, RouteMap, etc) whose align
     property set to (alTop, alLeft, alTop, alBottom, alClient) will call
     this procedure to adjust their bounds. All navigation buttons would not
     call it because they do not have align set, so they can display at
     the bottom of the wizard. }
  inherited AdjustClientRect(Rect);
  if FButtonBarHeight > ciButtonHeight then
    Rect.Bottom := Rect.Bottom - FButtonBarHeight;
end;

{$IFDEF VCL}
procedure TJvWizard.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  Msg.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;
{$ENDIF VCL}

procedure TJvWizard.UpdateButtonsStatus;
var
  AKind: TJvWizardButtonKind;
  AEnabledButtonSet: TJvWizardButtonSet;
  AVisibleButtonSet: TJvWizardButtonSet;
begin
  AEnabledButtonSet := [bkCancel];
  AVisibleButtonSet := [bkBack, bkNext, bkCancel];
  if Assigned(FActivePage) then
  begin
    { YW - By default, the Back button should be disabled for the first
       page at run time }
    if not (csDesigning in ComponentState) and IsFirstPage(FActivePage) then
      Exclude(FActivePage.FEnabledButtons, bkBack);
    AEnabledButtonSet := FActivePage.FEnabledButtons;
    AVisibleButtonSet := FActivePage.FVisibleButtons;
    if csDesigning in ComponentState then
    begin
      Include(AEnabledButtonSet, bkBack);
      Include(AEnabledButtonSet, bkNext);
      Include(AVisibleButtonSet, bkBack);
      Include(AVisibleButtonSet, bkNext);
      Exclude(AVisibleButtonSet, bkFinish);
    end;
  end;
  { YW - Change Buttons' status. }
  for AKind := Low(TJvWizardButtonKind) to High(TJvWizardButtonKind) do
  begin
    FNavigateButtons[AKind].Control.Visible := AKind in AVisibleButtonSet;
    FNavigateButtons[AKind].Control.Enabled := AKind in AEnabledButtonSet;
  end;
  { YW - Set Default Button, Next Button has the higher priority than
     the Finish Button. }
  if (bkNext in AVisibleButtonSet) and (bkNext in AEnabledButtonSet) then
    FNavigateButtons[bkNext].Control.Default := True
  else
  if (bkFinish in AVisibleButtonSet) and (bkFinish in AEnabledButtonSet) then
    FNavigateButtons[bkFinish].Control.Default := True;
end;

procedure TJvWizard.RepositionButtons;
var
  ATop: Integer;
  ALeft: Integer;
  AButtonSet: TJvWizardButtonSet;
  AButtonKind: TJvWizardButtonKind;

  procedure LocateButton(const AKind: TJvWizardButtonKind;
    const AOffset: Integer);
  begin
    if AKind in AButtonSet then
    begin
      with FNavigateButtons[AKind] do
      begin
        if FControl.Alignment = alRight then
          ALeft := ALeft - FControl.Width;
        FControl.SetBounds(ALeft, ATop, FControl.Width, ciButtonHeight);
        if FControl.Alignment = alLeft then
          ALeft := ALeft + FControl.Width;
      end;
      ALeft := ALeft + AOffset;
    end;
  end;

begin
  if FButtonBarHeight > ciButtonHeight then
  begin
    AButtonSet := [bkBack, bkNext, bkCancel];
    if Assigned(FActivePage) then
    begin
      AButtonSet := FActivePage.FVisibleButtons;
      if csDesigning in ComponentState then
      begin
        Include(AButtonSet, bkBack);
        Include(AButtonSet, bkNext);
        Exclude(AButtonSet, bkFinish);
      end;
    end;
    ATop := ClientRect.Bottom - FButtonBarHeight + ciButtonPlacement + 2;
    { YW - Position left side buttons }
    ALeft := ClientRect.Left + ciButtonPlacement;
    LocateButton(bkHelp, ciButtonPlacement + 2);
    LocateButton(bkStart, 1);
    LocateButton(bkLast, 0);
    { YW - Position right side buttons }
    ALeft := ClientRect.Right - ciButtonPlacement;
    if [bkNext, bkFinish] * AButtonSet = [bkNext, bkFinish] then
    begin
      LocateButton(bkCancel, -1);
      LocateButton(bkFinish, -ciButtonPlacement - 2);
    end
    else
    begin
      LocateButton(bkCancel, -ciButtonPlacement - 2);
      LocateButton(bkFinish, -1);
    end;
    LocateButton(bkNext, -2);
    LocateButton(bkBack, 0);
  end
  else // Hide all buttons
  begin
    for AButtonKind := Low(TJvWizardButtonKind) to High(TJvWizardButtonKind) do
    begin
      with FNavigateButtons[AButtonKind] do
        FControl.SetBounds(0, 0, FControl.Width, 0); // YW - Must keep the width
    end;
  end;
end;

procedure TJvWizard.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Control: TControl;
begin
  { YW - Force the wizard to load its pages in page order. }
  for I := 0 to FPages.Count - 1 do
    Proc(TComponent(FPages[I]));
  { YW - Load other controls, otherwise, those controls won't show up in
     the wizard. }
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    { YW - Because all the pages are already loaded, so here we do NOT need to
       load them again, otherwise it will cause 'duplicate component nam'
       error. }
    if not (Control is TJvWizardCustomPage) and (Control.Owner = Root) then
      Proc(Control);
  end;
end;

procedure TJvWizard.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FHeaderImages) then
    SetHeaderImages(nil);
end;

procedure TJvWizard.ImageListChange;
begin
  if HandleAllocated and (Sender = FHeaderImages) and Assigned(FActivePage) then
    FActivePage.Invalidate;
end;

procedure TJvWizard.ShowControl(AControl: TControl);
begin
  if (AControl is TJvWizardCustomPage) and
    (TJvWizardCustomPage(AControl).Wizard = Self) then
    SetActivePage(TJvWizardCustomPage(AControl));
  inherited ShowControl(AControl);
end;

procedure TJvWizard.Resize;
begin
  RepositionButtons;
  inherited Resize;
end;

function TJvWizard.IsForward(const FromPage, ToPage: TJvWizardCustomPage): Boolean;
begin
  if Assigned(FromPage) and Assigned(ToPage) and
    (FromPage.Wizard <> ToPage.Wizard) then
    raise EJvWizardError.CreateRes(@RsEInvalidWizardPage);
  Result := not Assigned(FromPage) or (Assigned(ToPage) and
    (FromPage.PageIndex < ToPage.PageIndex));
end;

procedure TJvWizard.SetAutoHideButtonBar(const Value: boolean);
begin
  if FAutoHideButtonBar <> Value then
  begin
    FAutoHideButtonBar := Value;
    RepositionButtons;
    UpdateButtonsStatus;
  end;
end;

function TJvWizard.GetWizardPages(Index: integer): TJvWizardCustomPage;
begin
  Result := TJvWizardCustomPage(Pages[Index]);
end;

procedure TJvWizard.DoActivePageChanged;
begin
  if Assigned(FOnActivePageChanged) then
    FOnActivePageChanged(Self);
end;

procedure TJvWizard.DoActivePageChanging(var ToPage: TJvWizardCustomPage);
begin
  if Assigned(FOnActivePageChanging) then
    FOnActivePageChanging(Self, ToPage);
end;

end.

