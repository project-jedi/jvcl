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
Michael Beck [mbeck@bigfoot.com].

Last Modified:  2003-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvgTypes;

interface

uses
   Windows,
   Graphics,
   ExtCtrls,
   StdCtrls,
   Controls,
   JVCLVer,
   ComCtrls;

type
   PFont = ^TFont;
   TglPercent = 0..100;
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
   //  TglProgressBorderStyle = ( fbsFlat, fbsCtl3D, fbsStatusControl,
   //			       fbsRaised, fbsRaisedFrame, fbsRecessedFrame );
   TglOrientation = (goHorizontal, goVertical);
   TPercentRange = 0..100;
   TglLabelOptions_ = (floActiveWhileControlFocused, floBufferedDraw,
      floDelineatedText, floIgnoreMouse, {floQuality3D,} floTransparentFont);
   TglLabelOptions = set of TglLabelOptions_;
   TglStTextOptions_ = (ftoActiveWhileControlFocused, ftoBroadwiseLastLine,
      ftoIgnoreMouse, ftoUnderlinedActive);
   TglStTextOptions = set of TglStTextOptions_;
   TglCBoxOptions_ = (fcoActiveWhileControlFocused, fcoBoldChecked,
      fcoEnabledFocusControlWhileChecked, fcoIgnoreMouse, fcoDelineatedText,
      {fcoQuality3D,} fcoFastDraw, fcoUnderlinedActive);
   TglCBoxOptions = set of TglCBoxOptions_;
   TglGrBoxOptions_ = (fgoCanCollapse, fgoCollapseOther, fgoFilledCaption,
      fgoFluentlyCollapse, fgoFluentlyExpand, fgoResizeParent,
      fgoHideChildrenWhenCollapsed, fgoIgnoreMouse, fgoDelineatedText,
      {fgoQuality3D,} fgoBufferedDraw, fgoOneAlwaysExpanded, fgoSaveChildFocus);
   TglGrBoxOptions = set of TglGrBoxOptions_;
   TglListBoxOptions_ = (fboAutoCtl3DColors, fboBufferedDraw,
      fboChangeGlyphColor, fboDelineatedText, fboExcludeGlyphs, fboHideText,
      fboHotTrack, fboHotTrackSelect, fboItemColorAsGradientFrom,
      fboItemColorAsGradientTo, fboMouseMoveSentensive, fboShowFocus,
      fboSingleGlyph, fboTransparent, fboWordWrap);
   TglListBoxOptions = set of TglListBoxOptions_;
   TglProgressOptions_ = (fpoDelineatedText, fpoTransparent);
   TglProgressOptions = set of TglProgressOptions_;
   TglTabOptions_ = (ftoAutoFontDirection, ftoExcludeGlyphs, ftoHideGlyphs,
      ftoInheriteTabFonts, ftoTabColorAsGradientFrom, ftoTabColorAsGradientTo,
      ftoWordWrap);
   TglTabOptions = set of TglTabOptions_;

   TglTreeViewOptions_ = (ftvFlatScroll);
   TglTreeViewOptions = set of TglTreeViewOptions_;

   TFocusControlMethod = (fcmOnMouseEnter, fcmOnMouseDown, fcmOnMouseUp);
   TProgressChangeEvent = procedure(Sender: TObject; Percent: Integer) of
      object;
   TglOnGetItemColorEvent = procedure(Sender: TObject; Index: integer; var
      Color: TColor) of object;

   //  TglDrawGlyphsOptions_ = ( fgoDefaultEnabled, fgoDefaultDisabled );
   //  TglDrawGlyphsOptions  = set of TglGlyphsOptions_;
   TglBoxStyle = (fbsFlat, fbsCtl3D, fbsStatusControl, fbsRecessed, fbsRaised,
      fbsRaisedFrame, fbsRecessedFrame);
   TglSide = (fsdLeft, fsdTop, fsdRight, fsdBottom);
   //  TBorders		  = set of TBorder;
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

   PDRAWITEMSTRUCT = ^TDRAWITEMSTRUCT;

   TglGlobalData = record
      fSuppressGradient: boolean;
      lp3DColors: Pointer;
   end;

   TJvgPublicWinControl = class(TWinControl)
   public
      procedure PaintWindow(DC: HDC); override;
      procedure RecreateWnd;
      property Font;
      property OnEnter;
      property OnExit;
      property Color;
   end;

   TJvgShowFont = class(TControl)
   public
      property Font;
   end;

   TJvCustomTreeView = class(TCustomTreeView)
   private
      FAboutJVCL: TJVCLAboutInfo;
   published
      property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored
         False;
   end;

const
   ALLGLSIDES                 = [fsdLeft, fsdTop, fsdRight, fsdBottom];
   { OEM Resource Ordinal Numbers }
   OBM_CLOSE                  = 32754;
   OBM_UPARROW                = 32753;
   OBM_DNARROW                = 32752;
   OBM_RGARROW                = 32751;
   OBM_LFARROW                = 32750;
   OBM_REDUCE                 = 32749;
   OBM_ZOOM                   = 32748;
   OBM_RESTORE                = 32747;
   OBM_REDUCED                = 32746;
   OBM_ZOOMD                  = 32745;
   OBM_RESTORED               = 32744;
   OBM_UPARROWD               = 32743;
   OBM_DNARROWD               = 32742;
   OBM_RGARROWD               = 32741;
   OBM_LFARROWD               = 32740;
   OBM_MNARROW                = 32739;
   OBM_COMBO                  = 32738;
   OBM_UPARROWI               = 32737;
   OBM_DNARROWI               = 32736;
   OBM_RGARROWI               = 32735;
   OBM_LFARROWI               = 32734;
   OBM_OLD_CLOSE              = 32767;
   OBM_SIZE                   = 32766;
   OBM_OLD_UPARROW            = 32765;
   OBM_OLD_DNARROW            = 32764;
   OBM_OLD_RGARROW            = 32763;
   OBM_OLD_LFARROW            = 32762;
   OBM_BTSIZE                 = 32761;
   OBM_CHECK                  = 32760;
   OBM_CHECKBOXES             = 32759;
   OBM_BTNCORNERS             = 32758;
   OBM_OLD_REDUCE             = 32757;
   OBM_OLD_ZOOM               = 32756;
   OBM_OLD_RESTORE            = 32755;

var                                     //...global variables
   glGlobalData               : TglGlobalData;

   //  fgcSUPRESSGRADIENTFILLING = $10000000;
   //  fgcUSEFR3DCOLORSDATACOMPONENT = $20000000;

implementation

procedure TJvgPublicWinControl.PaintWindow(DC: HDC);
begin
   inherited PaintWindow(DC);
end;

procedure TJvgPublicWinControl.RecreateWnd;
begin
   inherited RecreateWnd;
end;

initialization
   begin
      glGlobalData.fSuppressGradient := false;
      glGlobalData.lp3DColors := nil;
   end;

end.

