"""Example of creating the wire format in 'Example: A Simple XML Like Structure'"""

import pb_vector_pb2 as xpb


def main():
    xml = xpb.XMLFile(elements=[
        xpb.xml_element(
            name="space1",
            attributes=[
                xpb.kv_pair(key="foo", atom_value="1"),
                xpb.kv_pair(key="bar", atom_value="2")
            ],
            contents=[
                xpb.aux_xml_element(atom="fum"),
                xpb.aux_xml_element(atom="bar"),
                xpb.aux_xml_element(
                    element=xpb.xml_element(
                        name="space2",
                        attributes=[
                            xpb.kv_pair(key="fum", float_value=3.1415),
                            xpb.kv_pair(key="bum", int_value=-14)
                        ],
                        contents=[xpb.aux_xml_element(atom="more stuff for you")])),
                xpb.aux_xml_element(
                    element=xpb.xml_element(
                        name="space2b",
                        contents=[
                            xpb.aux_xml_element(atom="this"),
                            xpb.aux_xml_element(atom="is"),
                            xpb.aux_xml_element(atom="embedded"),
                            xpb.aux_xml_element(atom="also")
                        ])),
                xpb.aux_xml_element(atom="to"),
                xpb.aux_xml_element(atom="you")
            ])
    ])
    # print('xml:', xml)
    print(list(xml.SerializeToString()))


if __name__ == "__main__":
    main()
