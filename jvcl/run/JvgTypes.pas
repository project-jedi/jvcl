{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvgTypes.PAS, released on 2003-01-15.

The Initial Developer of the Original Code is Andrey V. Chudin,  [chudin@yandex.ru]
Portions created by Andrey V. Chudin are Copyright (C) 2003 Andrey V. Chudin.
All Rights Reserved.

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvgTypes;

interface

uses
  {$IFDEF VCL}
  Graphics;
  {$ENDIF VCL}
  {$IFDEF VisualCLX}
  Graphics;
  {$ENDIF VisualCLX}

{$IFDEF MSWINDOWS}
{$IFDEF VCL}
const
  { OEM Resource Ordinal Numbers }
  OBM_CLOSE       = 32754;
  OBM_UPARROW     = 32753;
  OBM_DNARROW     = 32752;
  OBM_RGARROW     = 32751;
  OBM_LFARROW     = 32750;
  OBM_REDUCE      = 32749;
  OBM_ZOOM        = 32748;
  OBM_RESTORE     = 32747;
  OBM_REDUCED     = 32746;
  OBM_ZOOMD       = 32745;
  OBM_RESTORED    = 32744;
  OBM_UPARROWD    = 32743;
  OBM_DNARROWD    = 32742;
  OBM_RGARROWD    = 32741;
  OBM_LFARROWD    = 32740;
  OBM_MNARROW     = 32739;
  OBM_COMBO       = 32738;
  OBM_UPARROWI    = 32737;
  OBM_DNARROWI    = 32736;
  OBM_RGARROWI    = 32735;
  OBM_LFARROWI    = 32734;
  OBM_OLD_CLOSE   = 32767;
  OBM_SIZE        = 32766;
  OBM_OLD_UPARROW = 32765;
  OBM_OLD_DNARROW = 32764;
  OBM_OLD_RGARROW = 32763;
  OBM_OLD_LFARROW = 32762;
  OBM_BTSIZE      = 32761;
  OBM_CHECK       = 32760;
  OBM_CHECKBOXES  = 32759;
  OBM_BTNCORNERS  = 32758;
  OBM_OLD_REDUCE  = 32757;
  OBM_OLD_ZOOM    = 32756;
  OBM_OLD_RESTORE = 32755;
{$ENDIF VCL}
{$ENDIF MSWINDOWS}

type
  TSpPercent = 1..99;
  TglItemsDrawStyle = (idsNone, idsRecessed, idsRaised);
  TglWallpaperOption = (fwoNone, fwoStretch, fwoPropStretch, fwoTile);
  TglDrawState = (fdsDefault, fdsDisabled, fdsDelicate);
  TglVertAlign = (fvaTop, fvaCenter, fvaBottom);
  TglHorAlign = (fhaLeft, fhaCenter, fhaRight);
  TglSizingDir = (fsdIncrease, fsdDecrease);
  TglScalingDir = (fsdRaising, fsdRecessing);
  TglTextStyle = (fstNone, fstRaised, fstRecessed, fstPushed, fstShadow,
    fstVolumetric);
  TglAutoTransparentColor = (ftcUser, ftcLeftTopPixel, ftcLeftBottomPixel,
    ftcRightTopPixel, ftcRightBottomPixel);
  TglGradientDir = (fgdHorizontal, fgdVertical, fgdLeftBias, fgdRightBias,
    fgdRectangle, fgdHorzConvergent, fgdVertConvergent);
  TglLinesDir = (fldHorizontal, fldVertical, fldLeftBias, fldRightBias);
  TThreeDGradientType = (fgtFlat, fgt3D);
  //  TglGgradientColorOnStep = ( fgcIncrease, fgcDecrease );
  TglLabelDir = (fldLeftRight, fldRightLeft, fldUpDown, fldDownUp);
  TglAlignment = (ftaLeftJustify, ftaRightJustify, ftaCenter, ftaBroadwise);
  TFontWeight = (fwDONTCARE, fwTHIN, fwEXTRALIGHT, fwLIGHT, fwNORMAL, fwMEDIUM,
    fwSEMIBOLD, fwBOLD, fwEXTRABOLD, fwHEAVY);
  TglGlyphKind = (fgkCustom, fgkDefault);
  TglFileType = (fftUndefined, fftGif, fftJpeg, fftBmp);
  // TglProgressBorderStyle = (fbsFlat, fbsCtl3D, fbsStatusControl,
  //   fbsRaised, fbsRaisedFrame, fbsRecessedFrame);
  TPercentRange = 0..100;
  TglLabelOption = (floActiveWhileControlFocused, floBufferedDraw,
    floDelineatedText, floIgnoreMouse, {floQuality3D,} floTransparentFont);
  TglLabelOptions = set of TglLabelOption;
  TglStaticTextOption = (ftoActiveWhileControlFocused, ftoBroadwiseLastLine,
    ftoIgnoreMouse, ftoUnderlinedActive);
  TglStaticTextOptions = set of TglStaticTextOption;
  TglCheckBoxOption = (fcoActiveWhileControlFocused, fcoBoldChecked,
    fcoEnabledFocusControlWhileChecked, fcoIgnoreMouse, fcoDelineatedText,
    {fcoQuality3D,} fcoFastDraw, fcoUnderlinedActive);
  TglCheckBoxOptions = set of TglCheckBoxOption;
  TglGroupBoxOption = (fgoCanCollapse, fgoCollapseOther, fgoFilledCaption,
    fgoFluentlyCollapse, fgoFluentlyExpand, fgoResizeParent,
    fgoHideChildrenWhenCollapsed, fgoIgnoreMouse, fgoDelineatedText,
    {fgoQuality3D,} fgoBufferedDraw, fgoOneAlwaysExpanded, fgoSaveChildFocus);
  TglGroupBoxOptions = set of TglGroupBoxOption;
  TglListBoxOption = (fboAutoCtl3DColors, fboBufferedDraw,
    fboChangeGlyphColor, fboDelineatedText, fboExcludeGlyphs, fboHideText,
    fboHotTrack, fboHotTrackSelect, fboItemColorAsGradientFrom,
    fboItemColorAsGradientTo, fboMouseMoveSentensive, fboShowFocus,
    fboSingleGlyph, fboTransparent, fboWordWrap);
  TglListBoxOptions = set of TglListBoxOption;
  TglProgressOption = (fpoDelineatedText, fpoTransparent);
  TglProgressOptions = set of TglProgressOption;
  TglTabOption = (ftoAutoFontDirection, ftoExcludeGlyphs, ftoHideGlyphs,
    ftoInheriteTabFonts, ftoTabColorAsGradientFrom, ftoTabColorAsGradientTo,
    ftoWordWrap);
  TglTabOptions = set of TglTabOption;

  TglTreeViewOption = (ftvFlatScroll);
  TglTreeViewOptions = set of TglTreeViewOption;

  TFocusControlMethod = (fcmOnMouseEnter, fcmOnMouseDown, fcmOnMouseUp);
  TProgressChangeEvent = procedure(Sender: TObject; Percent: Integer) of object;
  TglOnGetItemColorEvent = procedure(Sender: TObject; Index: Integer; var Color: TColor) of object;

  //  TglDrawGlyphsOption = ( fgoDefaultEnabled, fgoDefaultDisabled );
  //  TglDrawGlyphsOptions  = set of TglGlyphsOptions;
  TglBoxStyle = (fbsFlat, fbsCtl3D, fbsStatusControl, fbsRecessed, fbsRaised,
    fbsRaisedFrame, fbsRecessedFrame);
  TglSide = (fsdLeft, fsdTop, fsdRight, fsdBottom);
  //  TBorders = set of TBorder;
  TglSides = set of TglSide;
  TglOrigin = (forLeftTop, forRightBottom);
  TglAlign = record
    Horizontal: TglHorAlign;
    Vertical: TglVertAlign;
  end;

  TglHComponentAlign = (haNoChange, haLeft, haCenters, haRight, haSpaceEqually,
    haCenterWindow, haClose);
  TglVComponentAlign = (vaNoChange, vaTops, vaCenters, vaBottoms,
    vaSpaceEqually, vaCenterWindow, vaClose);

  TglCheckKind = (fckCheckBoxes, fckRadioButtons);

  TglGlobalData = record
    fSuppressGradient: Boolean;
    lp3DColors: Pointer;
  end;

const
  ALLGLSIDES = [fsdLeft, fsdTop, fsdRight, fsdBottom];

// (rom) not very elegant should be removed

var //...global variables
  glGlobalData: TglGlobalData = (
    fSuppressGradient: False;
    lp3DColors: nil
  );

  //  fgcSUPRESSGRADIENTFILLING = $10000000;
  //  fgcUSEFR3DCOLORSDATACOMPONENT = $20000000;

implementation

end.

