package jackrabbit

import (
	"fmt"
	"strings"
)

var InterfacesLib = map[string]*IoInterfaceClass{
	"StreamIn": {
		Name:    "StreamIn",
		Signals: []string{"valid", "accept", "data"},
		Hls: &HlsInterface{
			Connector: "HlsStream",
			Role:      "reader",
			Aliases: map[string]string{
				"read_rdy":         "valid",
				"read_req":         "accept",
				"read_r_e_t_u_r_n": "data",
			},
		},
	},
	"StreamOut": {
		Name:    "StreamOut",
		Signals: []string{"valid", "accept", "data"},
		Hls: &HlsInterface{
			Connector: "HlsStream",
			Role:      "writer",
			Aliases: map[string]string{
				"write_rdy": "accept",
				"write_req": "valid",
				"val":       "data",
			},
		},
	},
}

type ITypeParam struct {
	Name string
}

type IMethodParam struct {
	Name string
	Mode string
	Type string
}

type IMethod struct {
	Name       string
	Type       string
	Parameters []*IMethodParam
	Roles      []string
	Ready      *int
	Cycles     *string
	Out        *string
}

type IConnector struct {
	Name       string
	Roles      []map[string]string
	Parameters []*ITypeParam
	Methods    []IMethod
}

var hlsStreamConnector = IConnector{
	Name: "HlsStream",
	Roles: []map[string]string{
		{"name": "reader", "min": "1", "max": "1"},
		{"name": "writer", "min": "1", "max": "1"},
	},
	Parameters: []*ITypeParam{
		{Name: "T"},
	},
	Methods: []IMethod{
		{
			Name:       "read",
			Type:       "T",
			Parameters: []*IMethodParam{},
			Out:        strPtr("val"),
			Roles:      []string{"reader"},
			Ready:      intPtr(1),
			Cycles:     strPtr("0"),
		},
		{
			Name: "write",
			Type: "void",
			Parameters: []*IMethodParam{
				{Name: "val", Mode: "input", Type: "T"},
			},
			Roles:  []string{"writer"},
			Ready:  intPtr(1),
			Cycles: strPtr("0"),
		},
	},
}

var hlsConnectors = map[string]*IConnector{
	hlsStreamConnector.Name: &hlsStreamConnector,
}

type ISpecialized struct {
	Temp *IConnector
	Args []any
	Spec string
}

type Connectors struct {
	Specialized map[string]ISpecialized
}

func NewConnectors() *Connectors {
	return &Connectors{Specialized: make(map[string]ISpecialized)}
}

func (c *Connectors) Specialize(name string, args []any) (string, string) {
	temp, exists := hlsConnectors[name]
	if !exists {
		panic(fmt.Sprintf("Undefined connector: %s", name))
	}

	spec := ""
	for i, arg := range args {
		if i > 0 {
			spec += "_"
		}
		spec += fmt.Sprintf("%v", arg)
	}
	if spec == "" {
		spec = "any"
	}

	key := fmt.Sprintf("%s:%s", name, spec)
	if _, ok := c.Specialized[key]; !ok {
		c.Specialized[key] = ISpecialized{
			Temp: temp,
			Args: args,
			Spec: spec,
		}
	}

	return name, spec
}

func genType(spec string, xout *XMLBuilder) {
	// placeholder - normally parseTypeSpec is used here
	xout.Ele("type", map[string]string{
		"type":   "int",
		"signed": "true",
		"width":  "32",
	})
}

func (c *Connectors) GenerateTarget(xout *XMLBuilder) {
	for _, temp := range hlsConnectors {
		xc := xout.Ele("connector", map[string]string{
			"class": temp.Name,
			"gen":   temp.Name,
		})
		for _, role := range temp.Roles {
			xc.Ele("role", role)
		}
		for _, meth := range temp.Methods {
			attrs := map[string]string{
				"name":  meth.Name,
				"roles": joinRoles(meth.Roles),
			}
			if meth.Cycles != nil {
				attrs["cycles"] = *meth.Cycles
			}
			if meth.Ready != nil {
				attrs["ready"] = intStr(*meth.Ready)
			}
			if meth.Out != nil {
				attrs["out"] = *meth.Out
			}
			xc.Ele("method", attrs)
		}
	}

	// Specialized connectors
	for _, inst := range c.Specialized {
		xs := xout.Ele("specialized", map[string]string{
			"name": inst.Temp.Name,
			"spec": inst.Spec,
		})
		typeMap := map[string]string{}
		for idx, arg := range inst.Args {
			paramName := inst.Temp.Parameters[idx].Name
			xp := xs.Ele("parameter", map[string]string{
				"name": paramName,
			})
			switch v := arg.(type) {
			case int:
				xp.Ele("const", map[string]string{"value": intStr(v)})
			case string:
				typeMap[paramName] = v
				genType(v, xp)
			}
		}

		for _, meth := range inst.Temp.Methods {
			retType := meth.Type
			if t, ok := typeMap[retType]; ok {
				retType = t
			}
			typ := "void"
			width := 0
			if retType != "void" {
				typ = "int" // or "unsigned int" based on parseTypeSpec
				width = 32  // placeholder
			}
			xm := xs.Ele("method", map[string]string{
				"name":  meth.Name,
				"type":  typ,
				"width": intStr(width),
			})
			for carg, param := range meth.Parameters {
				ptype := param.Type
				if t, ok := typeMap[ptype]; ok {
					ptype = t
				}
				xm.Ele("signal", map[string]string{
					"name":   param.Name,
					"type":   param.Mode,
					"signed": "true",
					"width":  "32",
					"carg":   intStr(carg),
				})
			}
			if retType != "void" && meth.Out != nil {
				xm.Ele("signal", map[string]string{
					"name":   *meth.Out,
					"type":   "return",
					"signed": "true",
					"width":  "32",
				})
			}
		}
	}
}

// Utility functions
func strPtr(s string) *string { return &s }
func intPtr(i int) *int       { return &i }
func joinRoles(roles []string) string {
	return strings.Join(roles, " ")
}

func uint32Str(a uint32) string {
	return fmt.Sprintf("%d", a)
}

func intStr(a int) string {
	return fmt.Sprintf("%d", a)
}

func boolStr(b bool) string {
	if b {
		return "true"
	} else {
		return "false"
	}
}
