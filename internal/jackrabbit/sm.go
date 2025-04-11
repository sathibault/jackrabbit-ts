package jackrabbit

//#cgo CFLAGS: -I../../../eda/synth/sm
//#cgo LDFLAGS: -L../../../eda/synth/sm -ljackrabbit-sm -ltcl8.6 -lstdc++ -lm
//#include <stdlib.h>
//#include <stdbool.h>
//#include "jackrabbit.h"
import "C"

import (
	"fmt"
	"io"
	"os"
	"unsafe"
)

type SmContext struct {
	api unsafe.Pointer
	Xic *XicGenerator
}

type HlsBlockSummary struct {
	Position int
	End      int
	BlockNo  uint
	Pipeline bool
	Stages   uint
}

func CreateSmContext(path string) *SmContext {
	xic := newXicGenerator()

	c_path := C.CString(path)
	defer C.free(unsafe.Pointer(c_path))

	ctx := &SmContext{
		api: C.NewEditContext(c_path),
		Xic: xic,
	}
	return ctx
}

func (sm *SmContext) Generate(proc *HlsProcGen) {
	sm.Xic.Generate(proc)
	sm.Xic.Dump(os.Stderr)
	os.Stderr.Sync()
	C.EcSetRoot(sm.api, sm.Xic.root.xml)
}

func (sm *SmContext) Analysis() []HlsBlockSummary {
	c_count := C.uint(0)
	c_detail := C.EcAnalyze(sm.api, &c_count, false)
	if c_detail == nil {
		return nil
	}

	defer C.free(unsafe.Pointer(c_detail))

	n := uint(c_count)
	c_slice := (*[256]C.struct_block_detail_s)(unsafe.Pointer(c_detail))[:n]
	detail := make([]HlsBlockSummary, n)
	for i := uint(0); i < n; i++ {
		detail[i].BlockNo = uint(c_slice[i].bno)
		detail[i].Position = int(c_slice[i].pos)
		detail[i].End = int(c_slice[i].end)
		detail[i].Pipeline = c_slice[i].pipeline != 0
		detail[i].Stages = uint(c_slice[i].stages)
	}
	return detail
}

func (sm *SmContext) RenderBlock(no uint) string {
	c_html := C.EcRenderBlock(sm.api, C.uint(no))
	defer C.free(unsafe.Pointer(c_html))
	html := C.GoString(c_html)
	return html
}

type XicGenerator struct {
	root       *XMLBuilder
	file       *XMLBuilder
	config     *XMLBuilder
	target     *XMLBuilder
	connectors *Connectors
}

func newXicGenerator() *XicGenerator {
	root := createXml().Ele("hmcc", nil)

	file := root.Ele("file", map[string]string{
		"name": "logic",
	})

	config := file.Ele("config", map[string]string{
		"name": "logic",
	})

	arch := root.Ele("arch", nil)

	target := arch.Ele("target", map[string]string{
		"name": "VHDL/target.xml",
	})

	arch.Ele("pe", map[string]string{
		"name":       "fpga0",
		"target":     "VHDL/target.xml",
		"technology": "VHDL/Unknown/unknown.xml",
	})

	return &XicGenerator{
		root:       root,
		file:       file,
		config:     config,
		target:     target,
		connectors: NewConnectors(),
	}
}

func (g *XicGenerator) Generate(proc *HlsProcGen) {
	proc.Generate(g)
	g.file.InsertBefore(proc.Root, g.config)
}

func (g *XicGenerator) Dump(out io.Writer) {
	fmt.Fprintln(out, g.root.String())
}

type XMLBuilder struct {
	xml    unsafe.Pointer
	Parent *XMLBuilder
}

func createXml() *XMLBuilder {
	return &XMLBuilder{}
}

func (x *XMLBuilder) TagName() string {
	s := C.XmlTagName(x.xml)
	return C.GoString(s)
}

func (x *XMLBuilder) Ele(name string, attr map[string]string) *XMLBuilder {
	if x.xml != nil {
		c := createXml().Ele(name, attr)
		x.Append(c)
		return c
	}
	x.xml = C.NewXmlElement(C.CString(name))
	for k, v := range attr {
		C.XmlSetAttribute(x.xml, C.CString(k), C.CString(v))
	}
	return x
}

func (x *XMLBuilder) Dat(cdata string) {
	C.NewXmlText(x.xml, C.CString(cdata))
}

func (x *XMLBuilder) Att(name, value string) {
	C.XmlSetAttribute(x.xml, C.CString(name), C.CString(value))
}

func (x *XMLBuilder) Append(child *XMLBuilder) {
	C.XmlAppend(x.xml, child.xml)
	child.Parent = x
}

func (x *XMLBuilder) InsertBefore(child *XMLBuilder, sibling *XMLBuilder) {
	C.XmlInsertBefore(x.xml, child.xml, sibling.xml)
	child.Parent = x
}

func (x *XMLBuilder) InsertAfter(child *XMLBuilder, sibling *XMLBuilder) {
	C.XmlInsertAfter(x.xml, child.xml, sibling.xml)
	child.Parent = x
}

// also detaches
func (x *XMLBuilder) Delete() {
	if x.xml != nil {
		C.XmlDetach(x.xml)
		C.XmlDelete(x.xml)
		x.xml = nil
	}
	x.Parent = nil
}

func (x *XMLBuilder) String() string {
	s := C.XmlString(x.xml)
	text := C.GoString(s)
	C.free(unsafe.Pointer(s))
	return text
}
