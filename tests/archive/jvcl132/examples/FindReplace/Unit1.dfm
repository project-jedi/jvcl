object Form1: TForm1
  Left = 388
  Top = 196
  Width = 436
  Height = 306
  Caption = 'FindReplace test/demo program'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 428
    Height = 260
    Align = alClient
    Lines.Strings = (
      'LICENSE AGREEMENT'
      ''
      
        'This product is provided to you free of charge. Should you accep' +
        't the terms of this '
      
        'agreement, [Company XXX] will grant you the personal and non-exc' +
        'lusive right to '
      
        'install and use the software. [Software XXX] or any other materi' +
        'als provided in this '
      
        'package may not be modified in any way. This evaluation version ' +
        'of [Software XXX] '
      
        'may be freely redistributed with all supplied files in its origi' +
        'nal executable archive '
      'format.'
      ''
      ''
      'COPYRIGHT NOTICE'
      ''
      
        '[Software XXX] and all other materials distributed with this pac' +
        'kage are Copyright (c) '
      '1997 by [Company XXX].'
      ''
      
        'No parts of [Software XXX] or other files provided in this packa' +
        'ge may be reproduced '
      
        'in part or in whole, except as provided in the License Agreement' +
        '. You may not remove '
      
        'any copyright or other proprietary notice from the software. You' +
        ' may not reverse '
      'engineer, decompile, or disassemble the software.'
      ''
      ''
      'DISCLAIMER'
      ''
      
        'This software is provided "AS IS". You agree that by accepting t' +
        'his license, you are '
      
        'expressly acknowledging that the use of this software is AT YOUR' +
        ' OWN RISK.'
      ''
      '[Company XXX] MAKES NO WARRANTY OF ANY KIND, EITHER EXPRESS OR '
      'IMPLIED, INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF '
      'MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, WITH '
      'RESPECT TO THIS SOFTWARE AND ACCOMPANYING DOCUMENTATION.'
      ''
      'IN NO EVENT SHALL [Company XXX] BE LIABLE FOR ANY DAMAGES '
      '(INCLUDING DAMAGES FOR LOSS OF BUSINESS PROFITS, BUSINESS '
      'INTERRUPTION, LOSS OF BUSINESS INFORMATION, OR OTHER PECUNIARY '
      
        'LOSS) ARISING OUT OF THE USE OF OR INABILITY TO USE THIS PROGRAM' +
        ', '
      
        'EVEN IF [Company XXX] HAS BEEN ADVISED OF THE POSSIBILITY OF SUC' +
        'H '
      'DAMAGES.'
      ''
      ''
      'TRADEMARKS'
      ''
      
        'Any product or brand names mentioned in this software and associ' +
        'ated manuals and '
      
        'files are trademarks or registered trademarks of their respectiv' +
        'e owners.'
      ''
      ''
      'YOU ACKNOWLEDGE THAT YOU HAVE READ THIS AGREEMENT, '
      'UNDERSTAND IT, AND AGREE TO BE BOUND BY ITS TERMS AND '
      'CONDITIONS. YOU FURTHER AGREE THAT IT IS THE COMPLETE AND '
      'EXCLUSIVE STATEMENT OF THE AGREEMENT BETWEEN US THAT '
      'SUPERSEDES ANY PROPOSAL OR PRIOR AGREEMENT, ORAL OR WRITTEN, '
      'AND ANY OTHER COMMUNICATIONS BETWEEN US RELATING TO THE '
      'SUBJECT MATTER OF THIS AGREEMENT.')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object FindReplace1: TJvFindReplace
    Fast = True
    Options = [frDown, frShowHelp]
    EditControl = Memo1
    KeepText = True
    OnFind = FindReplace1Find
    OnReplace = FindReplace1Replace
    OnNotFound = FindReplace1NotFound
    Left = 48
    Top = 104
  end
  object MainMenu1: TMainMenu
    Left = 80
    Top = 104
    object Search1: TMenuItem
      Caption = 'Find (&Dialogs)'
      object Find1: TMenuItem
        Caption = 'Find...'
        ShortCut = 16450
        OnClick = Find1Click
      end
      object Replace1: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16466
        OnClick = Replace1Click
      end
      object FindAgain1: TMenuItem
        Caption = 'Find Again'
        ShortCut = 114
        OnClick = FindAgain1Click
      end
    end
    object Serachown1: TMenuItem
      Caption = 'Find (&Own dialogs)'
      object Find2: TMenuItem
        Caption = 'Find...'
        ShortCut = 16454
        OnClick = Find2Click
      end
      object Replace2: TMenuItem
        Caption = 'Replace...'
        ShortCut = 16464
        OnClick = Replace2Click
      end
      object FindAgain2: TMenuItem
        Caption = 'Find Again'
        ShortCut = 115
        OnClick = FindAgain2Click
      end
    end
    object Options1: TMenuItem
      Caption = 'O&ptions'
      object Rememberlastsearch1: TMenuItem
        Caption = 'Remember last search'
        Checked = True
        OnClick = Rememberlastsearch1Click
      end
    end
  end
end
