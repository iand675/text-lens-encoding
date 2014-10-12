{-# LANGUAGE TypeFamilies #-}
module Data.Text.Encoding.Lens where
import Data.Text.Lens
import Control.Lens.Iso
import qualified Data.ByteString as S
import qualified Data.Text as S
import qualified Data.Text.Encoding as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L

class Encodable t where
  type Encoded t
  utf8    :: Iso' t (Encoded t)
  utf16LE :: Iso' t (Encoded t)
  utf16BE :: Iso' t (Encoded t)
  utf32LE :: Iso' t (Encoded t)
  utf32BE :: Iso' t (Encoded t)

instance Encodable S.Text where
  type Encoded S.Text = S.ByteString

  utf8 = iso S.encodeUtf8 S.decodeUtf8
  {-# INLINE utf8 #-}
  utf16LE = iso S.encodeUtf16LE S.decodeUtf16LE
  {-# INLINE utf16LE #-}
  utf16BE = iso S.encodeUtf16BE S.decodeUtf16BE
  {-# INLINE utf16BE #-}
  utf32LE = iso S.encodeUtf32LE S.decodeUtf32LE
  {-# INLINE utf32LE #-}
  utf32BE = iso S.encodeUtf32BE S.decodeUtf32BE
  {-# INLINE utf32BE #-}

instance Encodable L.Text where
  type Encoded L.Text = L.ByteString

  utf8 = iso L.encodeUtf8 L.decodeUtf8
  {-# INLINE utf8 #-}
  utf16LE = iso L.encodeUtf16LE L.decodeUtf16LE
  {-# INLINE utf16LE #-}
  utf16BE = iso L.encodeUtf16BE L.decodeUtf16BE
  {-# INLINE utf16BE #-}
  utf32LE = iso L.encodeUtf32LE L.decodeUtf32LE
  {-# INLINE utf32LE #-}
  utf32BE = iso L.encodeUtf32BE L.decodeUtf32BE
  {-# INLINE utf32BE #-}

