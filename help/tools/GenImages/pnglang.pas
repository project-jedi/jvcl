{Portable Network Graphics Delphi Language Info (24 July 2002)}

{Feel free to change the text bellow to adapt to your language}
{Also if you have a translation to other languages and want to}
{share it, send me: gubadaud@terra.com.br                     }
unit pnglang;

interface

{$DEFINE English}
{.$DEFINE Portuguese}
{.$DEFINE German}

{Language strings for english}
resourcestring
  {$IFDEF English}
  EPngInvalidCRCText = 'This "Portable Network Graphics" image is not valid ' +
      'because it contains invalid pieces of data (crc error)';
  EPNGInvalidIHDRText = 'The "Portable Network Graphics" image could not be ' +
      'loaded because one of its main piece of data (ihdr) might be corrupted';
  EPNGMissingMultipleIDATText = 'This "Portable Network Graphics" image is ' +
    'invalid because it has missing image parts.';
  EPNGZLIBErrorText = 'Could not decompress the image because it contains ' +
    'invalid compressed data.'#13#10 + ' Description: ';
  EPNGInvalidPaletteText = 'The "Portable Network Graphics" image contains ' +
    'an invalid palette.';
  EPNGInvalidFileHeaderText = 'The file being readed is not a valid '+
    '"Portable Network Graphics" image because it contains an invalid header.' +
    ' This file may be corruped, try obtaining it again.';
  EPNGIHDRNotFirstText = 'This "Portable Network Graphics" image is not ' +
    'supported or it might be invalid.'#13#10 + '(IHDR chunk is not the first)';
  EPNGNotExistsText = 'The png file could not be loaded because it does not ' +
    'exists.';
  EPNGSizeExceedsText = 'This "Portable Network Graphics" image is not ' +
    'supported because either it''s width or height exceeds the maximum ' +
    'size, which is 65535 pixels length.';
  EPNGUnknownPalEntryText = 'There is no such palette entry.';
  EPNGMissingPaletteText = 'This "Portable Network Graphics" could not be ' +
    'loaded because it uses a color table which is missing.';
  EPNGUnknownCriticalChunkText = 'This "Portable Network Graphics" image ' +
    'contains an unknown critical part which could not be decoded.';
  EPNGUnknownCompressionText = 'This "Portable Network Graphics" image is ' +
    'encoded with an unknown compression scheme which could not be decoded.';
  EPNGUnknownInterlaceText = 'This "Portable Network Graphics" image uses ' +
    'an unknown interlace scheme which could not be decoded.';
  EPNGCannotAssignChunkText = 'The chunks must be compatible to be assigned.';
  EPNGUnexpectedEndText = 'This "Portable Network Graphics" image is invalid ' +
    'because the decoder found an unexpected end of the file.';
  EPNGNoImageDataText = 'This "Portable Network Graphics" image contains no ' +
    'data.';
  EPNGCannotChangeSizeText = 'The "Portable Network Graphics" image can not ' +
    'be resize by changing width and height properties. Try assigning the ' +
    'image from a bitmap.';
  EPNGCannotAddChunkText = 'The program tried to add a existent critical ' +
    'chunk to the current image which is not allowed.';
  EPNGCannotAddInvalidImageText = 'It''s not allowed to add a new chunk ' +
    'because the current image is invalid.';
  EPNGCouldNotLoadResourceText = 'The png image could not be loaded from the ' +
    'resource ID.';
  EPNGOutMemoryText = 'Some operation could not be performed because the ' +
    'system is out of resources. Close some windows and try again.';
  EPNGCannotChangeTransparentText = 'Setting bit transparency color is not ' +
    'allowed for png images containing alpha value for each pixel ' +
    '(COLOR_RGBALPHA and COLOR_GRAYSCALEALPHA)';
  EPNGHeaderNotPresentText = 'This operation is not valid because the ' +
    'current image contains no valid header.';
  EPNGAlphaNotSupportedText = 'The current "Portable Network Graphics" image ' +
    'color type either contains already alpha information or it can not ' +
    'be converted.';
  {$ENDIF}
  {$IFDEF Portuguese}
  EPngInvalidCRCText = 'Essa imagem "Portable Network Graphics" não é válida ' +
      'porque contém chunks inválidos de dados (erro crc)';
  EPNGInvalidIHDRText = 'A imagem "Portable Network Graphics" não pode ser ' +
      'carregada porque um dos seus chunks importantes (ihdr) pode estar '+
      'inválido';
  EPNGMissingMultipleIDATText = 'Essa imagem "Portable Network Graphics" é ' +
    'inválida porque tem chunks de dados faltando.';
  EPNGZLIBErrorText = 'Não foi possível descomprimir os dados da imagem ' +
    'porque ela contém dados inválidos.'#13#10 + ' Descrição: ';
  EPNGInvalidPaletteText = 'A imagem "Portable Network Graphics" contém ' +
    'uma paleta inválida.';
  EPNGInvalidFileHeaderText = 'O arquivo sendo lido não é uma imagem '+
    '"Portable Network Graphics" válida porque contém um cabeçalho inválido.' +
    ' O arquivo pode estar corrompida, tente obter ela novamente.';
  EPNGIHDRNotFirstText = 'Essa imagem "Portable Network Graphics" não é ' +
    'suportada ou pode ser inválida.'#13#10 + '(O chunk IHDR não é o ' +
    'primeiro)';
  EPNGNotExistsText = 'A imagem png não pode ser carregada porque ela não ' +
    'existe.';
  EPNGSizeExceedsText = 'Essa imagem "Portable Network Graphics" não é ' +
    'suportada porque a largura ou a altura ultrapassam o tamanho máximo, ' +
    'que é de 65535 pixels de diâmetro.';
  EPNGUnknownPalEntryText = 'Não existe essa entrada de paleta.';
  EPNGMissingPaletteText = 'Essa imagem "Portable Network Graphics" não pode ' +
    'ser carregada porque usa uma paleta que está faltando.';
  EPNGUnknownCriticalChunkText = 'Essa imagem "Portable Network Graphics" ' +
    'contém um chunk crítico desconheçido que não pode ser decodificado.';
  EPNGUnknownCompressionText = 'Essa imagem "Portable Network Graphics" está ' +
    'codificada com um esquema de compressão desconheçido e não pode ser ' +
    'decodificada.';
  EPNGUnknownInterlaceText = 'Essa imagem "Portable Network Graphics" usa um ' +
    'um esquema de interlace que não pode ser decodificado.';
  EPNGCannotAssignChunkText = 'Os chunk devem ser compatíveis para serem ' +
    'copiados.';
  EPNGUnexpectedEndText = 'Essa imagem "Portable Network Graphics" é ' +
    'inválida porque o decodificador encontrou um fim inesperado.';
  EPNGNoImageDataText = 'Essa imagem "Portable Network Graphics" não contém ' +
    'dados.';
  EPNGCannotChangeSizeText = 'A imagem "Portable Network Graphics" não pode ' +
    'ser redimensionada mudando as propriedades width e height. Tente ' +
    'copiar a imagem de um bitmap usando a função assign.';
  EPNGCannotAddChunkText = 'O programa tentou adicionar um chunk crítico ' +
    'já existente para a imagem atual, oque não é permitido.';
  EPNGCannotAddInvalidImageText = 'Não é permitido adicionar um chunk novo ' +
    'porque a imagem atual é inválida.';
  EPNGCouldNotLoadResourceText = 'A imagem png não pode ser carregada apartir' +
    ' do resource.';
  EPNGOutMemoryText = 'Uma operação não pode ser completada porque o sistema ' +
    'está sem recursos. Fecha algumas janelas e tente novamente.';
  EPNGCannotChangeTransparentText = 'Definir transparência booleana não é ' +
    'permitido para imagens png contendo informação alpha para cada pixel ' +
    '(COLOR_RGBALPHA e COLOR_GRAYSCALEALPHA)';
  EPNGHeaderNotPresentText = 'Essa operação não é válida porque a ' +
    'imagem atual não contém um cabeçalho válido.';
  EPNGAlphaNotSupportedText = 'A imagem "Portable Network Graphics" ou já ' +
    'contém informações de transparência, ou não suporta a conversão.';
  {$ENDIF}
  {Language strings for German}
  {$IFDEF German}
  EPngInvalidCRCText = 'Dieses "Portable Network Graphics" Image ist ' +
      'ungültig, weil Teile der Daten ungültig sind (CRC-Fehler).';
  EPNGInvalidIHDRText = 'Dieses "Portable Network Graphics" Image konnte ' +
      'nicht geladen werden, weil eine der Hauptdaten (IHDR) beschädigt ' +
      'sein könnte.';
  EPNGMissingMultipleIDATText = 'Dieses "Portable Network Graphics" Image ' +
    'ist ungültig, weil Grafikdaten fehlen.';
  EPNGZLIBErrorText = 'Die Grafik konnte nicht entpackt werden, weil sie ' +
    'fehlerhafte komprimierte Daten enthält.'#13#10 + ' Beschreibung: ';
  EPNGInvalidPaletteText = 'Das "Portable Network Graphics" Image enthält ' +
    'eine ungültige Palette.';
  EPNGInvalidFileHeaderText = 'Die Datei, die gelesen wird, ist kein ' +
    'gültiges "Portable Network Graphics" Image, da es keinen gültigen ' +
    'Header enthält. Die Datei könnte beschädigt sein, versuchen Sie, ' +
    'eine neue Kopie zu bekommen.';
  EPNGIHDRNotFirstText = 'Dieses "Portable Network Graphics" Image wird ' +
    'nicht unterstützt bzw. es könnte ungültig sein.'#13#10 +
    '(Der IHDR-Chunk ist nicht der erste Chunk in der Datei).';
  EPNGNotExistsText = 'Die PNG Datei konnte nicht geladen werden, da sie ' +
    'nicht existiert.';
  EPNGSizeExceedsText = 'Dieses "Portable Network Graphics" Image wird nicht ' +
    'unterstützt, weil entweder seine Breite oder seine Höhe das Maximum von ' +
    '65535 Pixeln überschreitet.';
  EPNGUnknownPalEntryText = 'Es gibt keinen solchen Palettenwert.';
  EPNGMissingPaletteText = 'Dieses "Portable Network Graphics" Image konnte ' +
    'nicht geladen werden, weil die benötigte Farbtabelle fehlt.';
  EPNGUnknownCriticalChunkText = 'Dieses "Portable Network Graphics" Image ' +
    'enhält einen unbekannten kritischen Teil, welcher nicht entschlüsselt ' +
    'werden kann.';
  EPNGUnknownCompressionText = 'Dieses "Portable Network Graphics" Image ' +
    'wurde mit einem unbekannten Komprimierungsalgorithmus kodiert, welcher ' +
    'nicht entschlüsselt werden kann.';
  EPNGUnknownInterlaceText = 'Dieses "Portable Network Graphics" Image ' +
    'benutzt ein unbekanntes Interlace-Schema, welcher nicht entschlüsselt ' +
    'werden kann.';
  EPNGCannotAssignChunkText = 'Die Chunks müssen kompatibel sein, um ' +
    'zugewiesen werden zu können.';
  EPNGUnexpectedEndText = 'Dieses "Portable Network Graphics" Image ist ' +
    'ungültig, der Dekoder stieß unerwarteterweise auf das Ende der Datei.';
  EPNGNoImageDataText = 'Dieses "Portable Network Graphics" Image enthält ' +
    'keine Daten.';
  EPNGCannotChangeSizeText = 'Das "Portable Network Graphics" Image kann ' +
    'nicht durch Ändern der Eigenschaften Width und Height in seinen ' +
    'Abmessungen geändert werden. Versuchen Sie das Image von einer Bitmap ' +
    'aus zuzuweisen.';
  EPNGCannotAddChunkText = 'Das Programm versucht einen existierenden ' +
    'kritischen Chunk zum aktuellen Image hinzuzufügen. Dies ist nicht ' +
    'zulässig.';
  EPNGCannotAddInvalidImageText = 'Es ist nicht zulässig, dem aktuellen ' +
    'Image einen neuen Chunk hinzuzufügen, da es ungültig ist.';
  EPNGCouldNotLoadResourceText = 'Das PNG Image konnte nicht von den ' +
    'Resourcendaten geladen werden.';
  EPNGOutMemoryText = 'Es stehen nicht genügend Resourcen im System zur ' +
    'Verfügung, um die Operation auszuführen. Schließen Sie einige Fenster '+
    'und versuchen Sie es erneut.';
  EPNGCannotChangeTransparentText = 'Das Setzen der Bit-' +
    'Transparent-Farbe ist fuer PNG-Images die Alpha-Werte fuer jedes ' +
    'Pixel enthalten (COLOR_RGBALPHA und COLOR_GRAYSCALEALPHA) nicht ' +
    'zulaessig';
  EPNGHeaderNotPresentText = 'Die Datei, die gelesen wird, ist kein ' +
    'gültiges "Portable Network Graphics" Image, da es keinen gültigen ' +
    'Header enthält.';
  EPNGAlphaNotSupportedText = 'The current "Portable Network Graphics" image ' +
    'color type either contains already alpha information or it can not ' +
    'be converted.';
  {$ENDIF}

implementation

end.
