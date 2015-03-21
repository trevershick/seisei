<editor-help>

    <style scoped>
        .help {
              width:100%;
              height:50%;
              overflow-y:auto;
              padding-left:10px;
              z-index:100;
                background-color: #222;
                border-top: 1px solid rgb(78, 93, 108);
                border-left:none;
                
        }
        h1 {
            font-size:16px;
        }
        .tag-samples {
            list-style: none;
            margin-right: 10px
        }
        .tag {
            padding-left: 10px;
            margin-right: 10px;
            margin-top: 10px;
            border-left: 2px solid #df691a;
        }
        .samples {
            padding-left:10px;
            border-left: 2px solid #df691a;
        }
        .sample {
            height:16px;
            font-family:monospace;
            font-size:10px;
        }
        .input {
            float: left;
            padding-left:10px;
            color:yellow;
        }
        .input a {
            color:yellow;
        }
        .output {
            float: right;   
            padding-right:10px;
            color:lime;
        }
        .other-samples {
            list-style: none;
            margin-right: 10px

        }
        .other-samples .input {

        }
        .other-samples .output {
            float:left;
            padding-left:10px;
        }
    </style>

    <div class="help">
        <h1>Directives</h1>

        <ul>
        <li each={this.tagSamples} class="tag-samples">
            <div class="tag">{name}</div>

            <div class="samples">
                <div class="sample" each={samples}>
                    <div class="input">
                    <a onclick={parent.parent.onSampleClick}>
                    {JSON.stringify(input)}
                    </a>
                    </div>
                    <div class="output">{JSON.stringify(output)}</div>
                </div>
            </div>            
        </li>
        </ul>
        <h1>Other Examples</h1>
        <ul>
        <li each={this.examples} class="other-samples">
            <div class="tag">{name}</div>

            <div class="samples">
                <div class="sample" each={samples}>
                    <div class="input">
                    <a onclick={parent.parent.onSampleClick}>
                    {JSON.stringify(input,null,2)}
                    </a></div>
                    <div class="output">{JSON.stringify(output,null,2)}</div>
                </div>
            </div>            
        </li>
        </ul>
    </div>

        onSampleClick(e) {
            this.opts.onsampleclick(e.item);
        }

        this.on('mount', function() {
            this.examples = [
            {
                name: "Mixed Tags",
                samples: [
                    { input: {address:"{{integer(100, 999)}} {{street()}}, {{city()}}, {{state()}}, {{integer(100, 10000)}}"},
                      output: {address:"269 W Bishop Ford I57 Xr, Carlsbad, KS, 4301"}
                    }
                ]
            }
            ];
            this.tagSamples = [
            {
                name:"repeat",
                samples: [
                    { input: { c: ["{{repeat(3)}}",{x:1}] }, output: { c: [{x: 1},{x: 1},{x: 1}]}}
                ]
            },
            {
                name:"random",
                samples: [
                    {input: { c: "{{random('blue', 'brown', 'green')}}"}, output: {c: 1}}
                ]
            },
            {
                name:"index",
                samples: [
                    { input: { c: ["{{repeat(3)}}",{x:"{{index}}"}] }, output: { c: [{x: 0},{x: 1},{x: 2}]}}
                ]
            },
            {
                name:"integer",
                samples: [
                    { input: { c:"{{integer}}"}, output: {c:7} },
                    { input: { c:"{{integer(1,100)}}"}, output: {c:7} }
                ]
            },
            {
                name:'date',
                signature: '([from], [to], [format])',
                samples: [
                    { input: { c:"{{date}}"}, output: {c:7} },
                    { input: { c:"{{date(20140101)}}"}, output: {c:7} },
                    { input: { c:"{{date(20140101,20991231)}}"}, output: {c:7} },
                    { input: { c:"{{date('20150317','20150317','yyyy-MM-dd')}}"}, output: {c:7} },
                    { input: { c:"{{date(20110317,'today','yyyy-MM-dd')}}"}, output: {c:7} }
                ]
            },
            {
                name:"company",
                samples: [
                    { input: { c:"{{company}}"}, output: {c:"Intel"} }
                ]
            },
            {
                name:"firstName",
                samples: [
                    { input: { c:"{{firstName}}"}, output: {c:"Terry"} },
                    { input: { c:'{{firstName(\'male\')}}'}, output: {c:'John'} },
                    { input: { c:'{{firstName(\'female\')}}'}, output: {c:'Linda'} }
                ]
            },
            {
                name:'surname',
                samples: [
                    { input: { c:'{{surname}}'}, output: {c:'Johnson'} }
                ]
            },
            {
                name:'guid',
                samples: [
                    { input: { c:'{{guid}}'}, output: {c:'32b85f72-9388-4dab-a7bd-dfe7435f1727'} }
                ]
            },
            {
                name:'objectId',
                samples: [
                    { input: { c:'{{objectId}}'}, output: {c:'32b85f72-9388-4dab-a7bd-dfe7435f1727'} }
                ]
            },
            {
                name:'street',
                samples: [
                    { input: { c:'{{street}}'}, output: {c:'N Trumbull Ave'} }
                ]
            },
            {
                name:'city',
                samples: [
                    { input: { c:'{{city}}'}, output: {c:'Clearlake'} }
                ]
            },
            {
                name:'state',
                samples: [
                    { input: { c:'{{state}}'}, output: {c:'IL'} }
                ]
            },
            {
                name:'zip',
                samples: [
                    { input: { c:'{{zip}}'}, output: {c:'60560'} }
                ]
            },
            {
                name:'bool',
                samples: [
                    { input: { c:'{{bool}}'}, output: {c:true} }
                ]
            },

        ];        
    });




</editor-help>
