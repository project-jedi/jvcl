object XMLBrowserDemo: TXMLBrowserDemo
  Left = 0
  Top = 0
  Caption = 'XML Browser Demo'
  ClientHeight = 646
  ClientWidth = 997
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 997
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    ParentBackground = False
    TabOrder = 0
    object Button1: TButton
      Left = 10
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 91
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Show Dialog'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 172
      Top = 10
      Width = 104
      Height = 25
      Caption = 'Transfer to bottom'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 997
    Height = 605
    Align = alClient
    Caption = 'Panel2'
    ParentBackground = False
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 1
      Top = 196
      Width = 995
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object Memo1: TMemo
      Left = 1
      Top = 1
      Width = 995
      Height = 195
      Align = alTop
      Lines.Strings = (
        '<?xml version="1.0" encoding="iso-8859-1"?>'
        '<Configuration>'
        '  <Program_Version>'
        '    <DownloadPasswordRequired>FALSE</DownloadPasswordRequired>'
        '    <LocalInstallerParams>TestParameter</LocalInstallerParams>'
        
          '    <ProgramLocationPath>Version Check\Remote\files</ProgramLoca' +
          'tionPath>'
        '    <ProgramLocationFileName>test.cmd</ProgramLocationFileName>'
        '    <ProgramVersion>1.9</ProgramVersion>'
        '    <ProgramReleaseType>prtAlpha</ProgramReleaseType>'
        '    <ProgramSize>20000</ProgramSize>'
        '    <ProgramReleaseDate>12.12.2004</ProgramReleaseDate>'
        '  </Program_Version>'
        '  <Program_Version>'
        '    <DownloadPasswordRequired>FALSE</DownloadPasswordRequired>'
        '    <LocalInstallerParams/>'
        
          '    <ProgramLocationPath>Version Check\Remote\files</ProgramLoca' +
          'tionPath>'
        '    <ProgramLocationFileName>Balls.exe</ProgramLocationFileName>'
        '    <ProgramVersion>1.8.1</ProgramVersion>'
        '    <ProgramReleaseType>prtBeta</ProgramReleaseType>'
        '    <ProgramSize>12000000</ProgramSize>'
        '    <ProgramReleaseDate>12.10.2004</ProgramReleaseDate>'
        '  </Program_Version>'
        '  <Program_Version>'
        '    <DownloadPasswordRequired>FALSE</DownloadPasswordRequired>'
        '    <LocalInstallerParams/>'
        
          '    <ProgramLocationPath>Version Check\Remote\files</ProgramLoca' +
          'tionPath>'
        '    <ProgramLocationFileName/>'
        '    <ProgramVersion>1.7</ProgramVersion>'
        '    <ProgramReleaseType>prtBeta</ProgramReleaseType>'
        '    <ProgramSize>12000000</ProgramSize>'
        '    <ProgramReleaseDate>10.10.2004</ProgramReleaseDate>'
        '  </Program_Version>'
        '  <Program_Version>'
        '    <DownloadPasswordRequired>FALSE</DownloadPasswordRequired>'
        '    <LocalInstallerParams/>'
        
          '    <ProgramLocationPath>Version Check\Remote\files</ProgramLoca' +
          'tionPath>'
        
          '    <ProgramLocationFileName>JvParameterListDemo.exe</ProgramLoc' +
          'ationFileName>'
        '    <ProgramVersion>1.6.1.1</ProgramVersion>'
        '    <VersionDescription>'
        '      <Item>Das ist ein test</Item>'
        '    </VersionDescription>'
        '    <ProgramReleaseType>prtProduction</ProgramReleaseType>'
        '    <ProgramSize>1000000</ProgramSize>'
        '    <ProgramReleaseDate>12.08.2004</ProgramReleaseDate>'
        '  </Program_Version>'
        
          '  <CurrentProductionProgramVersion>1.9</CurrentProductionProgram' +
          'Version>'
        '  <CurrentBetaProgramVersion>1.7</CurrentBetaProgramVersion>'
        
          '  <CurrentAlphaProgramVersion>1.6.1.1</CurrentAlphaProgramVersio' +
          'n>'
        '</Configuration>')
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object JvSimpleXML1: TJvSimpleXML
    IndentString = '  '
    Left = 245
    Top = 290
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    DefaultExt = 'xml'
    Filter = 'XML Files|*.xml|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 115
    Top = 290
  end
end
